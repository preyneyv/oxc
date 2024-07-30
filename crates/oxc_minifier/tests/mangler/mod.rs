use std::fmt::Write;

use oxc_allocator::Allocator;
use oxc_codegen::{CodeGenerator, CodegenOptions};
use oxc_mangler::ManglerBuilder;
use oxc_minifier::{CompressOptions, Minifier, MinifierOptions};
use oxc_parser::Parser;
use oxc_span::SourceType;

fn mangle(source_text: &str) -> String {
    let allocator = Allocator::with_capacity(source_text.len());
    let source_type = SourceType::default().with_module(true);
    let ret = Parser::new(&allocator, source_text, source_type).parse();
    let program = ret.program;
    let mangler = ManglerBuilder::default().build(&program);
    CodeGenerator::new().with_mangler(Some(mangler)).build(&program).source_text
}

#[test]
fn mangler() {
    let cases = [
        "function foo(a) {a}",
        "function foo(a) { let _ = { x } }",
        "function foo(a) { let { x } = y }",
        "var x; function foo(a) { ({ x } = y) }",
    ];

    let snapshot = cases.into_iter().fold(String::new(), |mut w, case| {
        write!(w, "{case}\n{}\n", mangle(case)).unwrap();
        w
    });

    insta::with_settings!({ prepend_module_to_snapshot => false, omit_expression => true }, {
        insta::assert_snapshot!("mangler", snapshot);
    });
}
