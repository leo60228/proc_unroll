//! `proc_unroll` is a proc macro to unroll loops inside a function. It supports loops of the
//! following forms:
//! * `for pat in int..int`
//! * `for pat in &[elem, elem]`
//!
//! Example:
//! ```
//! #[proc_unroll::unroll]
//! fn unrolled() -> Vec<u32> {
//!     let mut vec = Vec::new();
//!     for x in 10..20 {
//!         vec.push(x);
//!     }
//!     vec
//! }
//! assert_eq!(unrolled(), (10..20).collect::<Vec<_>>());
//! ```
use proc_macro2::{Span, TokenStream};
use proc_macro_error::*;
use quote::{quote_spanned, ToTokens};
use std::ops::Range;
use syn::{
    parse_macro_input, parse_quote,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
    Block, Expr, ExprArray, ExprForLoop, ExprLit, ExprRange, ExprReference, ExprUnary, ItemFn, Lit,
    LitInt, Stmt, UnOp,
};

struct Unroller {
    pub range: Range<isize>,
    pub map: Box<dyn Fn(isize) -> TokenStream>,
}

fn try_expr_to_isize(expr: &Expr) -> syn::parse::Result<(isize, String)> {
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Int(i), ..
        }) => Ok((i.base10_parse()?, i.suffix().to_string())),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => try_expr_to_isize(expr).map(|(x, y)| (-x, y)),
        _ => Err(syn::Error::new_spanned(expr, "not an integer")),
    }
}

fn syn_unwrap<T>(result: syn::parse::Result<T>) -> T {
    match result {
        Ok(x) => x,
        Err(err) => abort!(err),
    }
}

macro_rules! unwrap_prop {
    ($x:expr, $prop:ident, $($msg:tt)*) => {
        match &$x.$prop {
            Some(x) => x,
            None => abort!($x, $($msg)*),
        }
    };
}

impl Unroller {
    fn span(expr: &ExprForLoop) -> Span {
        let pat_span = expr.pat.span();
        let expr_span = expr.expr.span();

        pat_span.join(expr_span).unwrap_or(expr_span)
    }

    fn simple(range: &ExprRange) -> Self {
        let start_expr = unwrap_prop!(range, from, "range must be bounded");
        let end_expr = unwrap_prop!(range, to, "range must be bounded");
        let (start, start_suffix) = syn_unwrap(try_expr_to_isize(start_expr));
        let (end, end_suffix) = syn_unwrap(try_expr_to_isize(end_expr));
        let suffix = if start_suffix != "" && end_suffix != "" {
            if start_suffix == end_suffix {
                start_suffix
            } else {
                abort!(range, "type mismatch: {} != {}", start_suffix, end_suffix)
            }
        } else if start_suffix != "" {
            start_suffix
        } else {
            end_suffix
        };
        let span = range.span();
        let map = Box::new(move |idx| {
            let lit = LitInt::new(&format!("{}{}", idx, suffix), span);
            quote_spanned!(span=> #lit)
        });
        let range = start..end;
        Self { range, map }
    }

    fn slice(span: Span, array: &ExprArray) -> Self {
        let elems = array.elems.clone();
        let len = elems.len() as isize; // array lengths should always fit in an isize
        let range = 0..len;
        let map = Box::new(move |idx: isize| {
            let elem = &elems[idx as usize];
            quote_spanned!(span=> &#elem)
        });
        Self { range, map }
    }

    pub fn new(expr: &ExprForLoop) -> Self {
        match &*expr.expr {
            Expr::Range(range) => Self::simple(range),
            Expr::Reference(
                reference
                @ ExprReference {
                    mutability: None, ..
                },
            ) => {
                if let Expr::Array(arr) = &*reference.expr {
                    Self::slice(reference.span(), arr)
                } else {
                    abort!(expr, "can't be unrolled")
                }
            }
            _ => abort!(expr, "can't be unrolled"),
        }
    }

    fn unroll_iter(&self, idx: isize, expr: &ExprForLoop) -> TokenStream {
        let init = (self.map)(idx);
        let pat = &expr.pat;
        let body = &expr.body.stmts;
        let span = Self::span(expr);
        quote_spanned! {span=>
            let #pat = #init;
            #(#body)*
        }
    }

    pub fn unroll(&self, expr: &ExprForLoop) -> Block {
        let iter = self.range.clone().map(|idx| self.unroll_iter(idx, expr));

        // providing a span here isn't necessary since all tokens come from unroll_iter
        parse_quote! {{
            #(#iter)*
        }}
    }
}

fn unroll_loop(expr: &ExprForLoop) -> Block {
    let unroller = Unroller::new(expr);
    unroller.unroll(expr)
}

struct Unroll;

impl VisitMut for Unroll {
    fn visit_block_mut(&mut self, i: &mut Block) {
        let mut stmts = Vec::new();
        for stmt in &i.stmts {
            match stmt {
                Stmt::Expr(Expr::ForLoop(for_loop)) => stmts.extend(unroll_loop(&for_loop).stmts),
                Stmt::Semi(Expr::ForLoop(for_loop), _) => {
                    stmts.extend(unroll_loop(&for_loop).stmts)
                }
                other => stmts.push(other.clone()),
            }
        }
        *i = Block {
            brace_token: i.brace_token,
            stmts,
        };
    }
}

#[proc_macro_attribute]
#[proc_macro_error]
pub fn unroll(
    _attr: proc_macro::TokenStream,
    tokens: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(tokens as ItemFn);
    visit_mut::visit_item_fn_mut(&mut Unroll, &mut input);
    input.into_token_stream().into()
}
