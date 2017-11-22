#![feature(box_syntax)]
#![feature(box_patterns)]

use std::collections::HashMap;
use std::cell::Cell;
use std::rc::Rc;
use std::any::Any;

#[derive(Debug, Clone)]
enum Type {
    Number,
    Bool,
}

#[derive(Debug, Clone)]
enum Value {
    Number(i64),
    Bool(bool),
}

type Name = &'static str;

#[derive(Debug, Clone)]
enum Variable {
    Number(Cell<i64>),
    Bool(Cell<bool>),
}

#[derive(Debug, Clone)]
enum Expr {
    Constant(Value),
    Add(Box<Expr>, Box<Expr>), // e1 + e2
    LessThan(Box<Expr>, Box<Expr>), // e1 < e2
    Let(Name, Type, Box<Expr>, Box<Expr>), // let v::t = e1 in e2
    Get(Name), // v
    Set(Name, Box<Expr>), // v = e
    While(Box<Expr>, Box<Expr>), // while e1 { e2 }
}

fn interpret(env: &HashMap<Name, Variable>, expr: &Expr) -> Value {
    match *expr {
        Expr::Constant(ref value) => value.clone(),
        Expr::Add(ref expr1, ref expr2) => {
            let value1 = interpret(env, expr1);
            let value2 = interpret(env, expr2);
            match (value1, value2) {
                (Value::Number(number1), Value::Number(number2)) => Value::Number(
                    number1 + number2,
                ),
                _ => panic!("Type error!"),
            }
        }
        Expr::LessThan(ref expr1, ref expr2) => {
            let value1 = interpret(env, expr1);
            let value2 = interpret(env, expr2);
            match (value1, value2) {
                (Value::Number(number1), Value::Number(number2)) => Value::Bool(number1 < number2),
                _ => panic!("Type error!"),
            }
        } 
        Expr::Let(ref name, ref typ, ref expr1, ref expr2) => {
            let value = interpret(env, expr1);
            let mut env = env.clone();
            match (typ, value) {
                (&Type::Number, Value::Number(number)) => {
                    env.insert(name, Variable::Number(Cell::new(number)));
                    interpret(&env, expr2)
                }
                (&Type::Bool, Value::Bool(bool)) => {
                    env.insert(name, Variable::Bool(Cell::new(bool)));
                    interpret(&env, expr2)
                }
                _ => panic!("Type error!"),
            }
        }
        Expr::Get(ref name) => {
            match env.get(name).unwrap() {
                &Variable::Number(ref number_cell) => Value::Number(number_cell.get()),
                &Variable::Bool(ref bool_cell) => Value::Bool(bool_cell.get()),
            }
        }
        Expr::Set(ref name, ref expr) => {
            let value = interpret(env, expr);
            match (env.get(name).unwrap(), &value) {
                (&Variable::Number(ref number_cell), &Value::Number(number)) => {
                    number_cell.set(number);
                    value
                }
                (&Variable::Bool(ref bool_cell), &Value::Bool(bool)) => {
                    bool_cell.set(bool);
                    value
                }
                _ => panic!("Type error!"),
            }
        }
        Expr::While(ref expr1, ref expr2) => {
            while true {
                match interpret(env, expr1) {
                    Value::Bool(true) => {
                        interpret(env, expr2);
                    }
                    Value::Bool(false) => break,
                    _ => panic!("Type error!"),
                }
            }
            Value::Bool(false)
        }
    }
}

#[derive(Debug, Clone)]
enum StagedVariable {
    Number(Rc<Cell<i64>>),
    Bool(Rc<Cell<bool>>),
}

enum Staged {
    Number(Box<Fn() -> i64>),
    Bool(Box<Fn() -> bool>),
}

fn stage(env: &HashMap<Name, StagedVariable>, expr: &Expr) -> Staged {
    match *expr {
        Expr::Constant(Value::Number(number)) => Staged::Number(box move || number),
        Expr::Constant(Value::Bool(bool)) => Staged::Bool(box move || bool),
        Expr::Add(ref expr1, ref expr2) => {
            let staged1 = stage(env, expr1);
            let staged2 = stage(env, expr2);
            match (staged1, staged2) {
                (Staged::Number(number1), Staged::Number(number2)) => Staged::Number(box move || {
                    number1() + number2()
                }),
                _ => panic!("Type error!"),
            }
        }
        Expr::LessThan(ref expr1, ref expr2) => {
            let staged1 = stage(env, expr1);
            let staged2 = stage(env, expr2);
            match (staged1, staged2) {
                (Staged::Number(number1), Staged::Number(number2)) => Staged::Bool(box move || {
                    number1() < number2()
                }),
                _ => panic!("Type error!"),
            }
        } 
        Expr::Let(ref name, ref typ, ref expr1, ref expr2) => {
            let staged1 = stage(&env, expr1);
            let mut env = env.clone();
            let staged_let: Box<Fn()> = match typ {
                &Type::Number => {
                    match staged1 {
                        Staged::Number(number) => {
                            let number_cell = Rc::new(Cell::new(0));
                            env.insert(name, StagedVariable::Number(number_cell.clone()));
                            box move || number_cell.set(number())
                        }
                        _ => panic!("Type error!"),
                    }
                }
                &Type::Bool => {
                    match staged1 {
                        Staged::Bool(bool) => {
                            let bool_cell = Rc::new(Cell::new(false));
                            env.insert(name, StagedVariable::Bool(bool_cell.clone()));
                            box move || bool_cell.set(bool())
                        }
                        _ => panic!("Type error!"),
                    }
                }
            };
            match stage(&env, expr2) {
                Staged::Number(number) => Staged::Number(box move || {
                    staged_let();
                    number()
                }),
                Staged::Bool(bool) => Staged::Bool(box move || {
                    staged_let();
                    bool()
                }),
            }
        }
        Expr::Get(ref name) => {
            match env.get(name).unwrap() {
                &StagedVariable::Number(ref number_cell) => {
                    let number_cell = number_cell.clone();
                    Staged::Number(box move || number_cell.get())
                }
                &StagedVariable::Bool(ref bool_cell) => {
                    let bool_cell = bool_cell.clone();
                    Staged::Bool(box move || bool_cell.get())
                }
            }
        }
        Expr::Set(ref name, ref expr) => {
            let staged = stage(env, expr);
            match env.get(name).unwrap() {
                &StagedVariable::Number(ref number_cell) => {
                    match staged {
                        Staged::Number(number) => {
                            let number_cell = number_cell.clone();
                            Staged::Number(box move || {
                                let number = number();
                                number_cell.set(number);
                                number
                            })
                        }
                        _ => panic!("Type error!"),
                    }
                }
                &StagedVariable::Bool(ref bool_cell) => {
                    match staged {
                        Staged::Bool(bool) => {
                            let bool_cell = bool_cell.clone();
                            Staged::Bool(box move || {
                                let bool = bool();
                                bool_cell.set(bool);
                                bool
                            })
                        }
                        _ => panic!("Type error!"),
                    }
                }
            }
        }
        Expr::While(ref expr1, ref expr2) => {
            match stage(env, expr1) {
                Staged::Bool(bool1) => {
                    Staged::Bool(match stage(env, expr2) {
                        Staged::Bool(bool2) => {
                            box move || {
                                while bool1() {
                                    bool2();
                                }
                                false
                            }
                        }
                        Staged::Number(number2) => {
                            box move || {
                                while bool1() {
                                    number2();
                                }
                                false
                            }
                        }
                    })
                }
                _ => panic!("Type error"),
            }
        }
    }
}

struct Staged2 {
    value: Box<Any>,
}

impl Staged2 {
    fn new<T, F>(staged: F) -> Self
    where
        F: Fn() -> T + 'static,
        T: 'static,
    {
        let box1: Box<F> = box staged;
        let box2: Box<Any> = box box1 as Box<Any>;
        Staged2 { value: box2 }
    }

    fn unwrap<T>(self) -> Box<Fn() -> T + 'static>
    where
        T: 'static,
    {
        match self.value.downcast().unwrap() {
            box box1 => box1,
        }
    }
}

trait Runnable {
    type Output;
    fn run(&self) -> Self::Output;
}

struct Constant<T> {
    value: T,
}

impl<T> Runnable for Constant<T>
where
    T: Copy,
{
    type Output = T;
    fn run(&self) -> T {
        self.value
    }
}

struct Add {
    expr1: Box<Runnable<Output = i64>>,
    expr2: Box<Runnable<Output = i64>>,
}

impl Runnable for Add {
    type Output = i64;
    fn run(&self) -> i64 {
        self.expr1.run() + self.expr2.run()
    }
}

struct LessThan {
    expr1: Box<Runnable<Output = i64>>,
    expr2: Box<Runnable<Output = i64>>,
}

impl Runnable for LessThan {
    type Output = bool;
    fn run(&self) -> bool {
        self.expr1.run() < self.expr2.run()
    }
}

struct Let<T1, T2> {
    cell: Rc<Cell<T1>>,
    expr1: Box<Runnable<Output = T1>>,
    expr2: Box<Runnable<Output = T2>>,
}

impl<T1, T2> Runnable for Let<T1, T2>
where
    T1: Copy,
{
    type Output = T2;
    fn run(&self) -> T2 {
        self.cell.set(self.expr1.run());
        self.expr2.run()
    }
}

struct Get<T> {
    cell: Rc<Cell<T>>,
}

impl<T> Runnable for Get<T>
where
    T: Copy,
{
    type Output = T;
    fn run(&self) -> T {
        self.cell.get()
    }
}

struct Set<T> {
    cell: Rc<Cell<T>>,
    expr: Box<Runnable<Output = T>>,
}

impl<T> Runnable for Set<T>
where
    T: Copy,
{
    type Output = T;
    fn run(&self) -> T {
        let output = self.expr.run();
        self.cell.set(output);
        output
    }
}

struct While<T> {
    expr1: Box<Runnable<Output = bool>>,
    expr2: Box<Runnable<Output = T>>,
}

impl<T> Runnable for While<T> {
    type Output = bool;
    fn run(&self) -> bool {
        while self.expr1.run() {
            self.expr2.run();
        }
        false
    }
}

struct StagedExpr {
    value: Box<Any>,
}

impl StagedExpr {
    fn new<T, R>(runnable: R) -> Self
    where
        R: Runnable<Output = T> + 'static,
        T: 'static,
    {
        let box1: Box<Runnable<Output = T>> = box runnable;
        let box2: Box<Any> = box box1 as Box<Any>;
        StagedExpr { value: box2 }
    }

    fn unwrap<T>(self) -> Box<Runnable<Output = T>>
    where
        T: 'static,
    {
        match self.value.downcast().unwrap() {
            box box1 => box1,
        }
    }
}

fn main() {
    // let i = 1 {
    //   while i < 1000 {
    //     i = i + 1
    //   }
    // }
    let expr = Expr::Let(
        "i",
        Type::Number,
        box Expr::Constant(Value::Number(1)),
        box Expr::While(
            box Expr::LessThan(box Expr::Get("i"), box Expr::Constant(Value::Number(1000))),
            box Expr::Set(
                "i",
                box Expr::Add(box Expr::Get("i"), box Expr::Constant(Value::Number(1))),
            ),
        ),
    );

    println!("{:?}", interpret(&HashMap::new(), &expr));
    if let Staged::Bool(bool) = stage(&HashMap::new(), &expr) {
        println!("{:?}", bool());
    }
}
