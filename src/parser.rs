use crate::ast_node::RootNode;

pub struct Parser<'a> {
    all_nodes: Vec<RootNode<'a>>,
}
fn push<'a>(parser: &mut Parser<'a>, node: RootNode<'a>) {
    parser.all_nodes.push(node);
}
#[must_use]
fn statement(parser: &Parser) {}
#[must_use]
fn variable_declaration(parser: &Parser) {}
#[must_use]
fn function_declaration(parser: &Parser) {}
#[must_use]
fn stack_block(parser: &Parser) {}
