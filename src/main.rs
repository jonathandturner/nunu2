use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Block {
    params: Vec<String>,
    commands: Vec<Element>,
}
impl Block {
    pub fn new() -> Self {
        Self {
            params: vec![],
            commands: vec![],
        }
    }

    pub fn get_free_variables(&self, known_variables: &mut Vec<String>) -> Vec<String> {
        let mut known_variables = known_variables.clone();
        known_variables.extend_from_slice(&self.params);

        let mut free_variables = vec![];
        for elem in &self.commands {
            free_variables.extend_from_slice(&elem.get_free_variables(&mut known_variables));
        }

        free_variables
    }
}

#[derive(Debug, Clone)]
pub enum Internal {
    Add,
}

#[derive(Debug, Clone)]
pub enum Element {
    Variable(String),
    Bare(String),
    Number(i64),
    Set(String, Box<Element>),
    Block(Block),
    Call(Vec<Element>),
}

impl Element {
    pub fn get_free_variables(&self, known_variables: &mut Vec<String>) -> Vec<String> {
        match self {
            Element::Variable(v) => {
                if known_variables.contains(v) {
                    vec![]
                } else {
                    vec![v.clone()]
                }
            }
            Element::Set(v, b) => {
                let box_free_variables = b.get_free_variables(known_variables);
                known_variables.push(v.to_string());

                box_free_variables
            }
            Element::Number(_) | Element::Bare(_) => vec![],
            Element::Block(v) => v.get_free_variables(known_variables),
            Element::Call(elems) => {
                let mut free_variables = vec![];
                for elem in elems {
                    free_variables.extend_from_slice(&elem.get_free_variables(known_variables));
                }
                free_variables
            }
        }
    }
}

pub type Captured = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Int(i64),
    String(String),
    Block(Block, Captured),
}

#[derive(Debug, Clone)]
enum EvalError {
    General(String),
}

struct ScopeRecord {
    variables: HashMap<String, Value>,
    commands: HashMap<String, Internal>,
}
impl ScopeRecord {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            commands: HashMap::new(),
        }
    }
}

struct Scope {
    records: Vec<ScopeRecord>,
}
impl Scope {
    pub fn new() -> Self {
        Self {
            records: vec![ScopeRecord::new()],
        }
    }
    pub fn get_variable(&self, name: &str) -> Option<Value> {
        for rec in self.records.iter().rev() {
            if let Some(v) = rec.variables.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
    pub fn get_command(&self, name: &str) -> Option<Internal> {
        for rec in self.records.iter().rev() {
            if let Some(v) = rec.commands.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
    pub fn add_variable(&mut self, name: &str, value: Value) {
        if let Some(rec) = self.records.last_mut() {
            let _ = rec.variables.insert(name.into(), value);
        }
    }
    pub fn add_command(&mut self, name: &str, internal: Internal) {
        if let Some(rec) = self.records.last_mut() {
            let _ = rec.commands.insert(name.into(), internal);
        }
    }
    pub fn enter_record(&mut self) {
        self.records.push(ScopeRecord::new());
    }
    pub fn exit_record(&mut self) {
        if self.records.len() > 1 {
            self.records.pop();
        }
    }
}

fn eval_block(elems: &[Element], scope: &mut Scope) -> Result<Value, EvalError> {
    let mut output = Value::Nothing;
    for elem in elems {
        output = eval(elem, scope)?;
    }

    Ok(output)
}

fn eval_call(elems: &[Element], scope: &mut Scope) -> Result<Value, EvalError> {
    match eval(&elems[0], scope)? {
        Value::Block(b, captured) => {
            // Run the block
            let mut args = vec![];
            for elem in elems.iter().skip(1) {
                args.push(eval(elem, scope)?);
            }

            if args.len() != b.params.len() {
                return Err(EvalError::General("Mismatched number of arguments".into()));
            }

            scope.enter_record();

            // Assign parameters to arguments
            for (arg, param) in args.iter().zip(b.params.iter()) {
                scope.add_variable(param, arg.clone());
            }

            // Assign free variables to captured values
            for (arg, val) in captured {
                scope.add_variable(&arg, val.clone());
            }

            // With the frame complete, run the block
            let output = eval_block(&b.commands, scope);

            scope.exit_record();

            output
        }
        Value::String(s) => {
            if let Some(i) = scope.get_command(&s) {
                match i {
                    Internal::Add => {
                        // Run the block
                        let mut args = vec![];
                        for elem in elems.iter().skip(1) {
                            args.push(eval(elem, scope)?);
                        }

                        if args.len() != 2 {
                            return Err(EvalError::General(
                                "Mismatched number of arguments".into(),
                            ));
                        }

                        match (&args[0], &args[1]) {
                            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 + i2)),
                            _ => Err(EvalError::General("Add expected integers".into())),
                        }
                    }
                }
            } else {
                Ok(Value::String("Ran an external command".into()))
            }
        }
        _ => Err(EvalError::General("Expected a command block".into())),
    }
}

fn capture_block(b: &Block, scope: &Scope) -> Result<Value, EvalError> {
    let mut known_variables = vec![];
    let free_variables = b.get_free_variables(&mut known_variables);

    let mut captured: Captured = HashMap::new();
    for free_variable in &free_variables {
        if let Some(v) = scope.get_variable(free_variable) {
            captured.insert(free_variable.into(), v.clone());
        } else {
            return Err(EvalError::General("Unknown variable".into()));
        }
    }

    Ok(Value::Block(b.clone(), captured))
}

fn eval(element: &Element, scope: &mut Scope) -> Result<Value, EvalError> {
    match element {
        Element::Variable(name) => {
            if let Some(v) = scope.get_variable(&name) {
                Ok(v)
            } else {
                Err(EvalError::General("Can not find variable".into()))
            }
        }
        Element::Number(n) => Ok(Value::Int(*n)),
        Element::Set(name, elem) => {
            if let Ok(v) = eval(elem, scope) {
                scope.add_variable(name, v);
                Ok(Value::Nothing)
            } else {
                Err(EvalError::General("Failed to eval rhs".into()))
            }
        }
        Element::Call(elems) => eval_call(elems, scope),
        Element::Block(b) => Ok(capture_block(b, scope)?),
        Element::Bare(s) => Ok(Value::String(s.clone())),
    }
}

fn main() {
    let mut scope = Scope::new();
    scope.add_variable("a", Value::Int(10));
    scope.add_command("add", Internal::Add);

    println!("{:?}", eval(&Element::Number(3), &mut scope));
    println!("{:?}", eval(&Element::Variable("a".into()), &mut scope));
    println!(
        "{:?}",
        eval(
            &Element::Set("b".into(), Box::new(Element::Number(11))),
            &mut scope
        )
    );
    println!("{:?}", eval(&Element::Variable("b".into()), &mut scope));

    let mut block = Block::new();
    block.params = vec!["c".into()];
    block.commands = vec![Element::Variable("c".into())];

    println!(
        "{:?}",
        eval(
            &Element::Call(vec![Element::Block(block), Element::Number(12)]),
            &mut scope
        )
    );

    println!(
        "{:?}",
        eval(
            &Element::Call(vec![
                Element::Bare("add".into()),
                Element::Variable("a".into()),
                Element::Number(100)
            ]),
            &mut scope
        )
    );

    let mut block2 = Block::new();
    block2.params = vec!["d".into()];
    block2.commands = vec![Element::Call(vec![
        Element::Bare("add".into()),
        Element::Variable("d".into()),
        Element::Variable("a".into()),
    ])];

    println!(
        "{:?}",
        eval(
            &Element::Set("myblock".into(), Box::new(Element::Block(block2))),
            &mut scope
        )
    );

    println!(
        "{:?}",
        eval(
            &Element::Call(vec![
                Element::Variable("myblock".into()),
                Element::Number(12)
            ]),
            &mut scope
        )
    );

    println!(
        "{:?}",
        eval(
            &Element::Set("a".into(), Box::new(Element::Number(1100))),
            &mut scope
        )
    );
    println!("{:?}", eval(&Element::Variable("a".into()), &mut scope));
    println!(
        "{:?}",
        eval(
            &Element::Call(vec![
                Element::Variable("myblock".into()),
                Element::Number(12)
            ]),
            &mut scope
        )
    );
    println!(
        "{:?}",
        eval(
            &Element::Call(vec![Element::Variable("myblock".into()),]),
            &mut scope
        )
    );
}
