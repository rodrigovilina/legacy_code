#ifndef EvaLLVM_h
#define EvaLLVM_h

#include <string>
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

class EvaLLVM {
  public:
    EvaLLVM() { moduleInit(); }

    void exec(const std::string& program) {
      compile();
      module->print(llvm::outs(), nullptr);
      saveModuleToFile("out.ll");
    }

  private:
    void compile(/* todo add ast */) {
      fn = create_function("main", llvm::FunctionType::get(builder->getInt32Ty(), false /* no var args */));

      auto result = gen(/* ast */);

      auto i32_result = builder->CreateIntCast(result, builder->getInt32Ty(), true);

      builder->CreateRet(i32_result);
    }

    void saveModuleToFile(const std::string& filename) {
      std::error_code error_code;
      llvm::raw_fd_ostream out(filename, error_code);
      module->print(out, nullptr);
    }

    llvm::Value* gen() {
      return builder->getInt32(42);
    }

    llvm::Function* create_function(const std::string& name, llvm::FunctionType* fn_type) {
      auto fn = module->getFunction(name);

      if (!fn) {
        fn = create_function_proto(name, fn_type);
      }

      create_function_block(fn);

      return fn;
    }

    llvm::Function* create_function_proto(const std::string& name, llvm::FunctionType* fn_type) {
      auto fn = llvm::Function::Create(fn_type, llvm::Function::ExternalLinkage, name, *module);
      verifyFunction(*fn);
      return fn;
    }

    void create_function_block(llvm::Function* fn) {
      auto entry = create_basic_block("entry", fn);
      builder->SetInsertPoint(entry);
    }

    llvm::BasicBlock* create_basic_block(std::string name, llvm::Function* fn = nullptr) {
      return llvm::BasicBlock::Create(*context, name, fn);
    }

    void moduleInit() {
      context = std::make_unique<llvm::LLVMContext>();
      module = std::make_unique<llvm::Module>("EvaLLVM", *context);
      builder = std::make_unique<llvm::IRBuilder<>>(*context);
    }

    llvm::Function* fn;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;
};

#endif
