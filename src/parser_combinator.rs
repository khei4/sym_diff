// mutable referenceにするとmoveした値を使うなと言われるんだけど, moveってshared referenceでも起こるのでは？

use super::expr::Env;

pub type ParseResult<'a, Output> = Result<(&'a str, &'a Env, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str, env: &'a Env) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str, env: &'a Env) -> ParseResult<'a, Output> {
        self.parser.parse(input, env)
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str, &'a Env) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a str, env: &'a Env) -> ParseResult<'a, Output> {
        self(input, env)
    }
}

// Basic Parser
pub fn identifier<'a>(input: &'a str, env: &'a Env) -> ParseResult<'a, (String, &'a Env)> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], env, (matched, env)))
}

pub fn any_char<'a>(input: &'a str, env: &'a Env) -> ParseResult<'a, (char, &'a Env)> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], env, (next, env))),
        _ => Err(input),
    }
}

// Parser Builder
pub fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str, env: &'a Env| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], env, ())),
        _ => Err(input),
    }
}

pub fn one_of<'a>(expected: Vec<&'static str>) -> impl Parser<'a, &'static str> {
    move |input: &'a str, env: &'a Env| {
        for &e in expected.iter() {
            match input.get(0..e.len()) {
                Some(next) if next == e => return Ok((&input[e.len()..], env, e)),
                _ => continue,
            }
        }
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, (char, &'a Env)> {
    pred(any_char, |c| c.0.is_whitespace())
}

pub fn space1<'a>() -> impl Parser<'a, Vec<(char, &'a Env)>> {
    one_or_more(whitespace_char())
}
pub fn space0<'a>() -> impl Parser<'a, Vec<(char, &'a Env)>> {
    zero_or_more(whitespace_char())
}
// pub fn space0<'a>() -> impl Parser<'a, (Vec<char>, &'a Env)> {
//     zero_or_more(whitespace_char()).map(|vec_c_r| {
//         let env = vec_c_r.last().expect("last environment get error");
//         (vec_c_r.iter().map(|(c, _e)| c).collect(), env)
//     })
// }

// Parser Combinator
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input, env| {
        parser
            .parse(input, env)
            .map(|(next_input, next_env, result)| (next_input, next_env, map_fn(result)))
    }
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input, env| {
        parser1
            .parse(input, env)
            .and_then(|(next_input, next_env, result1)| {
                parser2
                    .parse(next_input, next_env)
                    .map(|(last_input, last_env, result2)| {
                        (last_input, last_env, (result1, result2))
                    })
            })
    }
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

pub fn zero_or_one<'a, P, A>(parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |mut input, mut env| {
        let mut result = None;
        if let Ok((next_input, next_env, item)) = parser.parse(input, env) {
            env = next_env;
            input = next_input;
            result = Some(item);
        }
        Ok((input, env, result))
    }
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input, mut env| {
        let mut result = Vec::new();

        if let Ok((next_input, new_env, first_item)) = parser.parse(input, env) {
            env = new_env;
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }
        while let Ok((next_input, new_env, first_item)) = parser.parse(input, env) {
            env = new_env;
            input = next_input;
            result.push(first_item);
        }

        Ok((input, env, result))
    }
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input, mut env| {
        let mut result = Vec::new();

        while let Ok((next_input, next_env, first_item)) = parser.parse(input, env) {
            env = next_env;
            input = next_input;
            result.push(first_item);
        }

        Ok((input, env, result))
    }
}

pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input, env| {
        if let Ok((next_input, next_env, value)) = parser.parse(input, env) {
            if predicate(&value) {
                return Ok((next_input, next_env, value));
            }
        }
        Err(input)
    }
}

pub fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input, env| match parser1.parse(input, env) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input, env),
    }
}

pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input, env| match parser.parse(input, env) {
        Ok((next_input, next_env, result)) => f(result).parse(next_input, next_env),
        Err(err) => Err(err),
    }
}

pub fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}
