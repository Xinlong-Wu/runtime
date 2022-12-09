#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetSelect.h"

#include <stdlib.h>
#include<stdio.h>

using namespace llvm;

static llvm::MCDisassembler *gDisassembler = nullptr;
static llvm::MCSubtargetInfo *gSTI = nullptr;
static llvm::MCInstPrinter *gIP = nullptr;

void init_disasm(const char *triple) {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllDisassemblers();

  std::string errstr;
  std::string gTriple(triple);

  llvm::MCInstrInfo *gMII = nullptr;
  llvm::MCRegisterInfo *gMRI = nullptr;
  auto target = llvm::TargetRegistry::lookupTarget(gTriple, errstr);
  if (!target) {
    llvm::errs() << "Can't find target for " << gTriple << ": " << errstr << "\n";
    assert(0);
  }

  MCTargetOptions MCOptions;
  gSTI = target->createMCSubtargetInfo(gTriple, "", "");
  std::string isa = target->getName();
  if (isa == "riscv32" || isa == "riscv64") {
    gSTI->ApplyFeatureFlag("+m");
    gSTI->ApplyFeatureFlag("+a");
    gSTI->ApplyFeatureFlag("+c");
    gSTI->ApplyFeatureFlag("+f");
    gSTI->ApplyFeatureFlag("+d");
  }
  gMII = target->createMCInstrInfo();
  gMRI = target->createMCRegInfo(gTriple);
  auto AsmInfo = target->createMCAsmInfo(*gMRI, gTriple, MCOptions);
   auto llvmTripleTwine = Twine(triple);
   auto llvmtriple = llvm::Triple(llvmTripleTwine);
   auto Ctx = new llvm::MCContext(llvmtriple,AsmInfo, gMRI, nullptr);
  gDisassembler = target->createMCDisassembler(*gSTI, *Ctx);
  gIP = target->createMCInstPrinter(llvm::Triple(gTriple),
      AsmInfo->getAssemblerDialect(), *AsmInfo, *gMII, *gMRI);
  gIP->setPrintImmHex(true);
  gIP->setPrintBranchImmAsAddress(true);
}

void disassemble(uint8_t *code) {
  MCInst inst;
  llvm::ArrayRef<uint8_t> arr(code, 4);
  uint64_t dummy_size = 0;
  gDisassembler->getInstruction(inst, dummy_size, arr, 0, llvm::nulls());

  std::string s;
  raw_string_ostream os(s);
  gIP->printInst(&inst, 0, "", *gSTI, os);

  printf("%s\n",s.c_str());
}

int main(int argc, char const *argv[])
{
    init_disasm("riscv64");
    char str[20];
    printf("plesae input HEX insts or Ctrl+C to exit:\n");
    printf("Inst:\t");
    while(~scanf("%s\t%s\t%s\t%s",str,str+5,str+10,str+15)){
        uint8_t code[4];
        code[0] = strtol(str, NULL, 16);
        code[1] = strtol(str+5, NULL, 16);
        code[2] = strtol(str+10, NULL, 16);
        code[3] = strtol(str+15, NULL, 16);
        disassemble(code);
        printf("Inst:\t");
    }
    return 0;
}
