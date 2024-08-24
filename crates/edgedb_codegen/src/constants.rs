/// Query to get all types in the database.
pub const TYPES_QUERY: &str = r#"WITH
  MODULE schema,
  material_scalars := (
    SELECT ScalarType
    FILTER NOT .abstract
       AND NOT EXISTS .enum_values
       AND NOT EXISTS (SELECT .ancestors FILTER NOT .abstract)
  )

	SELECT Type {
	  id,
	  name :=
	    array_join(array_agg([IS ObjectType].union_of.name), ' | ')
	    IF EXISTS [IS ObjectType].union_of
	    ELSE .name,
	  is_abstract := .abstract,

	  kind := 'object' IF Type IS ObjectType ELSE
	          'scalar' IF Type IS ScalarType ELSE
	          'array' IF Type IS Array ELSE
	          'tuple' IF Type IS Tuple ELSE
	          'multirange' IF Type IS MultiRange ELSE
	          'unknown',

	  [IS ScalarType].enum_values,
	  is_seq := 'std::sequence' in [IS ScalarType].ancestors.name,
	  # for sequence (abstract type that has non-abstract ancestor)
	  single material_id := (
	    SELECT x := Type[IS ScalarType].ancestors
	    FILTER x IN material_scalars
	    LIMIT 1
	  ).id,

	  [IS InheritingObject].bases: {
	    id
	  } ORDER BY @index ASC,

	  [IS ObjectType].union_of,
	  [IS ObjectType].intersection_of,
	  [IS ObjectType].pointers: {
	    card := ('One' IF .required ELSE 'AtMostOne') IF <str>.cardinality = 'One' ELSE ('AtLeastOne' IF .required ELSE 'Many'),
	    name,
	    target_id := .target.id,
	    kind := 'link' IF .__type__.name = 'schema::Link' ELSE 'property',
	    is_exclusive := exists (select .constraints filter .name = 'std::exclusive'),
	    is_computed := len(.computed_fields) != 0,
	    is_readonly := .readonly,
	    has_default := EXISTS .default or ('std::sequence' in .target[IS ScalarType].ancestors.name),
	    [IS Link].pointers: {
	      card := ('One' IF .required ELSE 'AtMostOne') IF <str>.cardinality = "One" ELSE ('AtLeastOne' IF .required ELSE 'Many'),
	      name := '@' ++ .name,
	      target_id := .target.id,
	      kind := 'link' IF .__type__.name = 'schema::Link' ELSE 'property',
	      is_computed := len(.computed_fields) != 0,
	      is_readonly := .readonly
	    } filter .name != '@source' and .name != '@target',
	  } FILTER @is_owned,
	  exclusives := assert_distinct((
	    [is schema::ObjectType].constraints
	    union
	    [is schema::ObjectType].pointers.constraints
	  ) {
	    target := (.subject[is schema::Property].name ?? .subject[is schema::Link].name ?? .subjectexpr)
	  } filter .name = 'std::exclusive'),
	  backlinks := (
	     SELECT DETACHED Link
	     FILTER .target = Type
	       AND NOT EXISTS .source[IS ObjectType].union_of
	    ) {
	    card := 'AtMostOne'
	      IF
	      EXISTS (select .constraints filter .name = 'std::exclusive')
	      ELSE
	      'Many',
	    name := '<' ++ .name ++ '[is ' ++ assert_exists(.source.name) ++ ']',
	    stub := .name,
	    target_id := .source.id,
	    kind := 'link',
	    is_exclusive := (EXISTS (select .constraints filter .name = 'std::exclusive')) AND <str>.cardinality = 'One',
	  },
	  backlink_stubs := array_agg((
	    WITH
	      stubs := DISTINCT (SELECT DETACHED Link FILTER .target = Type).name,
	      baseObjectId := (SELECT DETACHED ObjectType FILTER .name = 'std::BaseObject' LIMIT 1).id
	    FOR stub in { stubs }
	    UNION (
	      SELECT {
	        card := 'Many',
	        name := '<' ++ stub,
	        target_id := baseObjectId,
	        kind := 'link',
	        is_exclusive := false,
	      }
	    )
	  )),
	  array_element_id := [IS Array].element_type.id,

	  tuple_elements := (SELECT [IS Tuple].element_types {
	    target_id := .type.id,
	    name
	  } ORDER BY @index ASC),
		 multirange_element_id := [IS MultiRange].element_type.id,
	}
ORDER BY .name;
"#;

/// Name of the input struct.
pub const INPUT_NAME: &str = "Input";
/// Name of the output struct.
pub const OUTPUT_NAME: &str = "Output";
/// Name of the query function.
pub const QUERY_NAME: &str = "query";
pub const CLIENT_NAME: &str = "client";
pub const PROPS_NAME: &str = "props";
