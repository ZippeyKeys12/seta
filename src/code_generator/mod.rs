use crate::{
    parsing::{
        CompilationUnit, Definition, Expression, FunctionDecl, SecurityType, ShapeType, Type,
        TypeDecl,
    },
    util::Stack,
};

use std::{collections::HashMap, error::Error, path::Path};

use anyhow::Result;

extern crate inkwell;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    support::LLVMString,
    targets::{InitializationConfig, Target},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::IntValue,
    OptimizationLevel,
};

pub struct CodeGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    execution_engine: ExecutionEngine<'a>,
    symbols: Vec<HashMap<String, &'a dyn BasicType<'a>>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new<'b>(
        context: &'b Context,
        module_name: &'b str,
    ) -> Result<CodeGenerator<'b>, LLVMString> {
        let module = context.create_module(module_name);
        let execution_engine = module.create_execution_engine()?;

        // Return
        Ok(CodeGenerator {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
            symbols: vec![HashMap::new()],
        })
    }

    pub fn add_symbol(&mut self, key: String, symbol: &'a dyn BasicType<'a>) {
        self.symbols.peek_mut().unwrap().insert(key, symbol);
    }

    pub fn add_symbols(&mut self, symbols: HashMap<String, &'a dyn BasicType<'a>>) {
        for (k, symbol) in symbols {
            self.add_symbol(k, symbol)
        }
    }

    pub fn save(&self, path: &Path) -> bool {
        self.module.write_bitcode_to_path(path)
    }

    pub fn compile(&mut self, compilation_unit: &CompilationUnit) {
        for d in &compilation_unit.definitions {
            if let Definition::Type(typ) = d {
                self.compile_type_decl(typ)
            }
        }
    }

    fn compile_function(&self, function: &FunctionDecl) {}

    fn compile_type_decl(&mut self, typ: &TypeDecl) {
        let name = typ.name.clone();
        let value = &typ.value;
        let typ = self.compile_type(value).unwrap();

        self.add_symbol(name, typ);
    }

    fn compile_type(&self, typ: &Type) -> Result<&'a dyn BasicType<'a>> {
        let scope = &self.symbols.peek().unwrap();

        match &*typ.shape {
            ShapeType::Reference(reference) => Ok(*scope.get(reference).unwrap()),
            _ => Err(anyhow!("Placeholder")),
        }
    }
}

pub fn compile(path: &Path, compilation_unit: CompilationUnit) -> Result<(), LLVMString> {
    let context = Context::create();

    // Build builtins
    let mut builtins: HashMap<String, &dyn BasicType> = HashMap::new();

    let int32 = context.i32_type();
    builtins.insert("Int".to_string(), &int32);

    // Load builtins
    let mut code_generator = CodeGenerator::new(&context, "main")?;
    code_generator.add_symbols(builtins);

    code_generator.compile(&compilation_unit);
    code_generator.save(path);

    Ok(())
}
