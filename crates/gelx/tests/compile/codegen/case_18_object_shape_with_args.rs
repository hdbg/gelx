pub mod example {
    use ::gelx::exports as __g;
    /// Execute the desired query.
    pub async fn query(
        client: &__g::gel_tokio::Client,
        props: &Input,
    ) -> ::core::result::Result<Vec<Output>, __g::gel_errors::Error> {
        client.query(QUERY, props).await
    }
    /// Compose the query as part of a larger transaction.
    pub async fn transaction(
        conn: &mut __g::gel_tokio::Transaction,
        props: &Input,
    ) -> ::core::result::Result<Vec<Output>, __g::gel_errors::Error> {
        conn.query(QUERY, props).await
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
        pub starts_with: String,
        #[builder(setter(into))]
        pub ends_with: String,
    }
    impl __g::gel_protocol::query_arg::QueryArgs for Input {
        fn encode(
            &self,
            encoder: &mut __g::gel_protocol::query_arg::Encoder,
        ) -> core::result::Result<(), __g::gel_errors::Error> {
            let map = __g::gel_protocol::named_args! {
                "starts_with" => self.starts_with.clone(), "ends_with" => self.ends_with
                .clone(),
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
    pub struct OutputWalletsSet {
        pub created_at: __g::DateTime,
        pub id: __g::uuid::Uuid,
        pub updated_at: __g::DateTime,
        pub primary: bool,
        pub description: Option<String>,
        pub name: Option<String>,
        pub pubkey: String,
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
        pub slug: String,
        pub id: __g::uuid::Uuid,
        pub created_at: __g::DateTime,
        pub updated_at: __g::DateTime,
        pub description: Option<String>,
        pub name: String,
        pub wallets: Vec<OutputWalletsSet>,
    }
    /// The original query string provided to the macro. Can be reused in your codebase.
    pub const QUERY: &str = "select Team {**} filter .name like <str>$starts_with ++ '%' and .description like '%' ++ <str>$ends_with;";
}
fn main() {}
