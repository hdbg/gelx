pub mod example {
    use ::gelx::exports as __g;
    /// Execute the desired query.
    pub async fn query(
        client: impl __g::gel_tokio::QueryExecutor,
        props: &Input,
    ) -> ::core::result::Result<Option<Output>, __g::gel_errors::Error> {
        client.query_single(QUERY, props).await
    }
    #[derive(
        ::std::fmt::Debug,
        ::core::clone::Clone,
        __g::serde::Serialize,
        __g::serde::Deserialize,
        __g::typed_builder::TypedBuilder,
        __g::gel_derive::Queryable
    )]
    #[builder(crate_module_path = __g::typed_builder)]
    #[gel(crate_path = __g::gel_protocol)]
    pub struct Input {
        #[builder(setter(into))]
        pub id: __g::uuid::Uuid,
    }
    impl __g::gel_protocol::query_arg::QueryArgs for Input {
        fn encode(
            &self,
            encoder: &mut __g::gel_protocol::query_arg::Encoder,
        ) -> core::result::Result<(), __g::gel_errors::Error> {
            let map = __g::gel_protocol::named_args! {
                "id" => self.id.clone(),
            };
            map.encode(encoder)
        }
    }
    #[derive(
        ::std::fmt::Debug,
        ::core::clone::Clone,
        __g::serde::Serialize,
        __g::serde::Deserialize,
        __g::gel_derive::Queryable
    )]
    #[gel(crate_path = __g::gel_protocol)]
    pub struct Output {
        pub id: __g::uuid::Uuid,
    }
    /// The original query string provided to the macro. Can be reused in your codebase.
    pub const QUERY: &str = "delete User filter .id = <uuid>$id;\n";
}
fn main() {}
