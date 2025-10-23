use crate::ast_node::TopLevelNode;
use crate::token::Token;

pub struct Parser<'a> {
    pub all_nodes: Vec<TopLevelNode<'a>>,
}

impl<'a> Parser<'a> {
    // utils
    fn push(&mut self, node: TopLevelNode<'a>) {
        self.all_nodes.push(node);
    }
    pub fn new() -> Parser<'a> {
        Parser{ all_nodes: Vec::new() }
    }
}

impl<'a> Parser<'a> { // recursive descent parser

    #[must_use]
    pub fn parse_tokens(&self, tokens: Vec<Token<'a>>) -> Vec<TopLevelNode<'a>> {
        todo!()
    }
    #[must_use]
    fn statement(&mut self) {}
    #[must_use]
    fn variable_declaration(&mut self) {}
    #[must_use]
    fn function_declaration(&mut self) {}
    #[must_use]
    fn stack_block(&mut self) {}
}
