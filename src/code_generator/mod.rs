use crate::{
    parsing::{
        CompilationUnit, Definition, Expression, FunctionDecl, SecurityType, ShapeType, Type,
        TypeDecl,
    },
    util::Stack,
};

use std::{collections::HashMap, path::Path};

use anyhow::Result;

extern crate inkwell;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
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
    pub fn new<'b>(context: &'b Context, module_name: &'b str) -> Result<CodeGenerator<'b>> {
        let module = context.create_module(module_name);
        let execution_engine = module
            .create_execution_engine()
            .map_err(|e| anyhow!(format!("{:?}", e)))?;

        // Return
        Ok(CodeGenerator {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
            symbols: vec![HashMap::new()],
        })
    }

    pub fn add_symbol(&mut self, key: String, symbol: &'a dyn BasicType<'a>) -> Result<()> {
        self.symbols
            .peek_mut()
            .ok_or_else(|| anyhow!("Scope is empty"))?
            .insert(key, symbol);

        Ok(())
    }

    pub fn add_symbols(&mut self, symbols: HashMap<String, &'a dyn BasicType<'a>>) -> Result<()> {
        for (k, symbol) in symbols {
            let res = self.add_symbol(k, symbol);

            if res.is_err() {
                return res;
            }
        }

        Ok(())
    }

    pub fn compile(&mut self, filename: &str, compilation_unit: &CompilationUnit) -> Result<()> {
        for d in &compilation_unit.definitions {
            if let Definition::Type(typ) = d {
                self.compile_type_decl(typ)?
            }
        }

        for d in &compilation_unit.definitions {
            if let Definition::Function(function) = d {
                self.compile_function(function)?
            }
        }

        Target::initialize_all(&InitializationConfig::default());

        let target_triple = TargetMachine::get_default_triple();

        let target =
            Target::from_triple(&target_triple).map_err(|e| anyhow!(format!("{:?}", e)))?;
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| anyhow!("Unable to create target machine!"))?;

        target_machine
            .write_to_file(&self.module, FileType::Object, filename.as_ref())
            .map_err(|e| anyhow!(format!("{:?}", e)))?;

        Ok(())
    }

    fn compile_function(&self, function: &FunctionDecl) -> Result<()> {
        let scope = &self.symbols.peek().unwrap();

        let FunctionDecl {
            doc,
            name,
            parameters,
            ret,
            body,
        } = function;

        let ret_type = self.compile_type(&ret.1)?;
        let fn_type = ret_type.fn_type(
            parameters
                .iter()
                .map(|(_, v)| self.compile_type(v).unwrap().as_basic_type_enum())
                .collect::<Vec<_>>()
                .as_slice(),
            false,
        );

        let name = name.as_str();
        let function = self.module.add_function(name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, y, name);
        self.builder.build_return(Some(&sum));

        // println!("a {}", name);
        // unsafe {
        //     let ex = self
        //         .execution_engine
        //         .get_function::<unsafe extern "C" fn(i32, i32) -> i32>("abc")?;
        //     println!("{}", ex.call(1, 2));
        // }
        // println!("b");

        Ok(())
    }

    fn compile_type_decl(&mut self, typ: &TypeDecl) -> Result<()> {
        let name = typ.name.clone();
        let value = &typ.value;
        let typ = self.compile_type(value).unwrap();

        self.add_symbol(name, typ);

        Ok(())
    }

    fn compile_type(&self, typ: &Type) -> Result<&'a dyn BasicType<'a>> {
        let scope = &self.symbols.peek().unwrap();

        match &*typ.shape {
            ShapeType::Reference(reference) => Ok(*scope.get(reference).unwrap()),
            _ => Err(anyhow!("Invalid Type (This should never be seen)")),
        }
    }
}

pub fn compile(path: &Path, compilation_unit: CompilationUnit) -> Result<()> {
    let context = Context::create();

    // Build builtins
    let mut builtins: HashMap<String, &dyn BasicType> = HashMap::new();

    let int32 = context.i32_type();
    builtins.insert("Int".to_string(), &int32);

    // Load builtins
    let mut code_generator = CodeGenerator::new(&context, "main")?;
    code_generator.add_symbols(builtins);

    let target_machine = code_generator.compile("a.out", &compilation_unit);

    Ok(())
}
