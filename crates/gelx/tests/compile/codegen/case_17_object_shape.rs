pub mod example {
    use ::gelx::exports as __g;
    /// Execute the desired query.
    pub async fn query(
        client: impl __g::gel_tokio::QueryExecutor,
    ) -> ::core::result::Result<Vec<Output>, __g::gel_errors::Error> {
        client.query(QUERY, &()).await
    }
    pub type Input = ();
    #[derive(
        ::std::fmt::Debug,
        ::core::clone::Clone,
        __g::serde::Serialize,
        __g::serde::Deserialize,
        __g::gel_derive::Queryable
    )]
    #[gel(crate_path = __g::gel_protocol)]
    pub struct OutputWalletsSet {
        pub created_at: __g::DateTimeAlias,
        pub id: __g::uuid::Uuid,
        pub updated_at: __g::DateTimeAlias,
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
        pub created_at: __g::DateTimeAlias,
        pub updated_at: __g::DateTimeAlias,
        pub description: Option<String>,
        pub name: String,
        pub wallets: Vec<OutputWalletsSet>,
    }
    /// The original query string provided to the macro. Can be reused in your codebase.
    pub const QUERY: &str = "select Team {**}";
}
fn main() {}
