//===--- Options.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

struct Options {
  let _bridged: BridgedPassContext

  var enableStackProtection: Bool {
    _bridged.enableStackProtection()
  }

  var enableMoveInoutStackProtection: Bool {
    _bridged.enableMoveInoutStackProtection()
  }

  var enableLifetimeDependenceDiagnostics: Bool {
    _bridged.enableLifetimeDependenceDiagnostics()
  }

  var enableLexicalLifetimes: Bool {
    _bridged.enableLexicalLifetimes()
  }

  var enableAsyncDemotion: Bool {
    _bridged.enableAsyncDemotion()
  }

  var enableDestroyHoisting: Bool {
    _bridged.enableDestroyHoisting()
  }

  var enableSpeculativeDevirtualization: Bool {
    _bridged.enableSpeculativeDevirtualization()
  }

  var enableExperimentalSwiftBasedClosureSpecialization: Bool {
    _bridged.enableExperimentalSwiftBasedClosureSpecialization()
  }

  var enablePackMetadataStackPromotion: Bool {
    _bridged.enablePackMetadataStackPromotion()
  }

  var enableOSSAModules: Bool {
    _bridged.enableOSSAModules()
  }

  enum CopyPropagationOption {
    // Do not add any copy propagation passes.
    case off

    // Only add the copy propagation passes requested by other flags, currently
    // just -enable-ossa-modules.
    case requestedPassesOnly

    // Add all relevant copy propagation passes.  If a setting, e.g.
    // -enable-ossa-modules, requests to add copy propagation to the pipeline, do so.
    case on
  }

  var copyPropagation: CopyPropagationOption {
    switch _bridged.copyPropagationOption() {
      case .Off:                 return .off
      case .RequestedPassesOnly: return .requestedPassesOnly
      case .On:                  return .on
      default: fatalError()
    }
  }

  func enableSimplification(for inst: Instruction) -> Bool {
    _bridged.enableSimplificationFor(inst.bridged)
  }

  var enableEmbeddedSwift: Bool {
    _bridged.hasFeature(.Embedded)
  }

  var stopOptimizationAfterSerialization: Bool {
    _bridged.stopOptimizationAfterSerialization()
  }

  var shouldOptimize: Bool {
    _bridged.shouldOptimize()
  }

  var assumeSingleThreaded: Bool {
    _bridged.assumeSingleThreaded()
  }

  func hasFeature(_ feature: BridgedFeature) -> Bool {
    _bridged.hasFeature(feature)
  }

  enum AssertConfiguration {
    case enabled
    case disabled
    case unknown
  }

  var assertConfiguration: AssertConfiguration {
    switch _bridged.getAssertConfiguration() {
      case .Debug:               return .enabled
      case .Release, .Unchecked: return .disabled
      default:                   return .unknown
    }
  }
}
