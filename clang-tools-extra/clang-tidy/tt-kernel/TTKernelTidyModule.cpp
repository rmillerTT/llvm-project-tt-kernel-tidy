//===--- MiscTidyModule.cpp - clang-tidy ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../ClangTidy.h"
#include "../ClangTidyModule.h"
#include "../ClangTidyModuleRegistry.h"
#include "CBReservePushBalanceCheck.h"
#include "CBPushPopBalanceCheck.h"

namespace clang::tidy {
namespace tt_kernel {

class TTKernelModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<CBReservePushBalanceCheck>(
        "tt-kernel-cb-reserve-push-balance");
    CheckFactories.registerCheck<CBPushPopBalanceCheck>(
        "tt-kernel-cb-push-pop-balance");
  }
};

} // namespace tt_kernel

// Register the TTKernelModule using this statically initialized variable.
static ClangTidyModuleRegistry::Add<tt_kernel::TTKernelModule>
    X("tt-kernel-module", "Adds tt-metal kernel lint checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the TTKernelModule.
volatile int TTKernelModuleAnchorSource = 0; // NOLINT(tt-kernel-use-internal-linkage)

} // namespace clang::tidy
