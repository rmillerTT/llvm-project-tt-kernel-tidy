set(LLVM_LINK_COMPONENTS
  FrontendOpenMP
  Support
  )

add_clang_library(clangTidyTTKernelModule STATIC
  CBReservePushBalanceCheck.cpp
  CBPushPopBalanceCheck.cpp
  TTKernelTidyModule.cpp

  LINK_LIBS
  clangTidy
  clangTidyUtils

  DEPENDS
  omp_gen
  ClangDriverOptions
  )

clang_target_link_libraries(clangTidyTTKernelModule
  PRIVATE
  clangAnalysis
  clangAST
  clangASTMatchers
  clangBasic
  clangFormat
  clangLex
  clangSerialization
  clangTooling
  )
