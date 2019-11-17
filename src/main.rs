use regex::Regex;
use std::str::FromStr;

#[derive(Debug)]
struct ParseError;

trait Grammar
where
    Self: Sized,
{
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError>;
}

trait Token {
    fn regex() -> &'static str;
}

impl<T> Grammar for T
where
    T: Token + Default,
{
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        let re: Regex = Regex::new(&("^".to_owned() + Self::regex())).unwrap();

        match re.find(txt) {
            None => Err(ParseError),
            Some(m) => Ok((Self::default(), &txt[m.end()..])),
        }
    }
}

impl Grammar for i32 {
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        let re: Regex = Regex::new("\\d+").unwrap();

        match re.find(txt) {
            None => Err(ParseError),
            Some(m) => Ok((
                FromStr::from_str(&txt[m.start()..m.end()]).unwrap(),
                &txt[m.end()..],
            )),
        }
    }
}

type Value = i32;

#[derive(Debug, Default, std::cmp::PartialEq)]
struct Add;
#[derive(Debug, Default, std::cmp::PartialEq)]
struct Sub;
#[derive(Debug, Default, std::cmp::PartialEq)]
struct Div;
#[derive(Debug, Default, std::cmp::PartialEq)]
struct Mul;
#[derive(Debug, std::cmp::PartialEq)]
enum BinaryOperator {
    Add(Add),
    Sub(Sub),
    Div(Div),
    Mul(Mul),
}

#[derive(Debug, std::cmp::PartialEq)]
struct BinaryExpression {
    left: Expression,
    op: BinaryOperator,
    right: Expression,
}

#[derive(Debug, std::cmp::PartialEq)]
enum Expression {
    Parenthesis(Box<Parenthesis>),
    BinaryExpression(BinaryExpression),
    Value(Value),
}

#[derive(Debug, std::cmp::PartialEq)]
struct Parenthesis(Expression);

impl Token for Add {
    fn regex() -> &'static str {
        "\\+"
    }
}

impl Token for Sub {
    fn regex() -> &'static str {
        "-"
    }
}

impl Token for Div {
    fn regex() -> &'static str {
        "[/÷]"
    }
}

impl Token for Mul {
    fn regex() -> &'static str {
        "[*×]"
    }
}

impl Grammar for BinaryOperator {
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        if let Ok((val, txt)) = Add::parse(txt) {
            return Ok((BinaryOperator::Add(val), txt));
        }
        if let Ok((val, txt)) = Sub::parse(txt) {
            return Ok((BinaryOperator::Sub(val), txt));
        }
        if let Ok((val, txt)) = Div::parse(txt) {
            return Ok((BinaryOperator::Div(val), txt));
        }
        if let Ok((val, txt)) = Mul::parse(txt) {
            return Ok((BinaryOperator::Mul(val), txt));
        }
        Err(ParseError)
    }
}

impl Grammar for BinaryExpression {
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        let (left, txt) = Expression::parse(txt)?;
        let (op, txt) = BinaryOperator::parse(txt)?;
        let (right, txt) = Expression::parse(txt)?;
        Ok((BinaryExpression { left, op, right }, txt))
    }
}

impl Grammar for Expression {
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        if let Ok((val, txt)) = Parenthesis::parse(txt) {
            return Ok((Expression::Parenthesis(Box::new(val)), txt));
        }
        if let Ok((val, txt)) = BinaryExpression::parse(txt) {
            return Ok((Expression::BinaryExpression(val), txt));
        }
        if let Ok((val, txt)) = Value::parse(txt) {
            return Ok((Expression::Value(val), txt));
        }
        Err(ParseError)
    }
}

#[derive(Debug, Default, std::cmp::PartialEq)]
struct LeftParenthesis;
impl Token for LeftParenthesis {
    fn regex() -> &'static str {
        "\\("
    }
}

#[derive(Debug, Default, std::cmp::PartialEq)]
struct RightParenthesis;
impl Token for RightParenthesis {
    fn regex() -> &'static str {
        "\\)"
    }
}

impl Grammar for Parenthesis {
    fn parse<'a>(txt: &'a str) -> Result<(Self, &'a str), ParseError> {
        let (_, txt) = LeftParenthesis::parse(txt)?;
        let (expression, txt) = Expression::parse(txt)?;
        let (_, txt) = RightParenthesis::parse(txt)?;
        return Ok((Parenthesis(expression), txt));
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    fn must_fail<T: Grammar>(txt: &str) {
        match T::parse(txt) {
            Ok(_) => assert!(false, "A pattern has been wrongly matched"),
            Err(_) => (),
        }
    }
    fn match_all<T>(txt: &str, expected_value: T)
    where
        T: Grammar + std::cmp::PartialEq + std::fmt::Debug,
    {
        match_partial(txt, expected_value, "")
    }
    fn match_partial<T>(txt: &str, expected_value: T, remaining_txt: &str)
    where
        T: Grammar + std::cmp::PartialEq + std::fmt::Debug,
    {
        if let Ok((val, txt)) = T::parse(txt) {
            assert_eq!(val, expected_value);
            assert_eq!(txt, remaining_txt);
        } else {
            assert!(false, "Pattern didn't match")
        }
    }

    #[test]
    fn literal() {
        match_all("18", 18);
        match_all::<i32>("18", 18);
        match_all::<Value>("18", 18);
        match_partial("18abc", 18, "abc");
        must_fail::<i32>("abc");
        must_fail::<Value>("abc");
    }

    #[test]
    fn add() {
        match_all("+", Add);
        match_partial("+abc", Add, "abc");
        must_fail::<Add>("abc");
    }

    #[test]
    fn sub() {
        match_all("-", Sub);
        match_partial("-abc", Sub, "abc");
        must_fail::<Sub>("abc");
    }

    #[test]
    fn mul() {
        match_all("*", Mul);
        match_all("×", Mul);
        match_partial("*abc", Mul, "abc");
        must_fail::<Mul>("abc");
    }

    #[test]
    fn div() {
        match_all("/", Div);
        match_all("÷", Div);
        match_partial("/abc", Div, "abc");
        must_fail::<Div>("abc");
    }

    #[test]
    fn binary_operator() {
        match_all::<BinaryOperator>("+", BinaryOperator::Add(Add));
        match_all::<BinaryOperator>("-", BinaryOperator::Sub(Sub));
        match_all::<BinaryOperator>("/", BinaryOperator::Div(Div));
        match_all::<BinaryOperator>("÷", BinaryOperator::Div(Div));
        match_all::<BinaryOperator>("*", BinaryOperator::Mul(Mul));
        match_all::<BinaryOperator>("×", BinaryOperator::Mul(Mul));
        match_partial::<BinaryOperator>("/abc", BinaryOperator::Div(Div), "abc");
        // must_fail::<BinaryOperator::Div>("abc");
    }

    #[test]
    fn binop() {
        match_all(
            "3÷4",
            BinaryExpression {
                left: 3,
                op: BinaryOperator::Div(Div),
                right: 4,
            },
        );
        match_all(
            "3÷(4)",
            BinaryExpression {
                left: 3,
                op: BinaryOperator::Div(Div),
                right: Expression::Value(Box::new(4)),
            },
        );
        match_all(
            "3÷(4-2)",
            BinaryExpression {
                left: 3,
                op: BinaryOperator::Div(Div),
                right: Expression::Parenthesis(Box::new(BinaryExpression {
                    left: 3,
                    op: BinaryOperator::Sub(Div),
                    right: 2,
                })),
            },
        );
        match_partial(
            "3÷4abc",
            BinaryExpression {
                left: 3,
                op: BinaryOperator::Div(Div),
                right: 4,
            },
            "abc",
        );
        must_fail::<BinaryExpression>("abc");
    }

    #[test]
    fn expression() {
        match_all::<Expression>("3", Expression::Value(3));
        match_all::<Expression>(
            "3+2",
            Expression::BinaryExpression(BinaryExpression {
                left: 3,
                op: BinaryOperator::Add(Add),
                right: 2,
            }),
        );
        match_partial("3abc", Expression::Value(3), "abc");
        match_partial(
            "3+2abc",
            Expression::BinaryExpression(BinaryExpression {
                left: 3,
                op: BinaryOperator::Add(Add),
                right: 2,
            }),
            "abc",
        );
        must_fail::<Expression>("abc");
    }

    #[test]
    fn parenthesis() {
        match_all("(3)", Parenthesis(Expression::Value(3)));
        match_all(
            "(3+2)",
            Parenthesis(Expression::BinaryExpression(BinaryExpression {
                left: 3,
                op: BinaryOperator::Add(Add),
                right: 2,
            })),
        );
        match_all(
            "((3))",
            Parenthesis(Expression::Parenthesis(Box::new(Parenthesis(
                Expression::Value(3),
            )))),
        );
        match_all(
            "((3+2))",
            Parenthesis(Expression::Parenthesis(Box::new(Parenthesis(
                Expression::BinaryExpression(BinaryExpression {
                    left: 3,
                    op: BinaryOperator::Add(Add),
                    right: 2,
                }),
            )))),
        );
        match_all(
            "((3+(2)))",
            Parenthesis(Expression::Parenthesis(Box::new(Parenthesis(
                Expression::BinaryExpression(BinaryExpression {
                    left: 3,
                    op: BinaryOperator::Add(Add),
                    right: 2,
                }),
            )))),
        );
        match_partial("(3)abc", Parenthesis(Expression::Value(3)), "abc");
        match_partial(
            "(3+2)abc",
            Parenthesis(Expression::BinaryExpression(BinaryExpression {
                left: 3,
                op: BinaryOperator::Add(Add),
                right: 2,
            })),
            "abc",
        );
        must_fail::<Parenthesis>("abc");
        must_fail::<Parenthesis>("(");
        must_fail::<Parenthesis>("()");
        must_fail::<Parenthesis>("(3");
        must_fail::<Parenthesis>("((3)");
    }
}

fn main() {
    println!("Hello, world !");
}
