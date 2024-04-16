
// RUN: %target-run-simple-swift(%S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature RawLayout -enable-experimental-feature Embedded -enable-experimental-feature FixedArrays -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O %S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature RawLayout -enable-experimental-feature Embedded -enable-experimental-feature FixedArrays -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// TODO: also test in non-embedded mode once this is supported

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

struct Const10: ConstantInt {
  static var value: Int { 10 }
}

struct Const3: ConstantInt {
  static var value: Int { 3 }
}

struct User {
  var a = SmallVector<Int, Const10>()
}

struct SmallVector<Element, InlineSize: ConstantInt> {
  var inlineArray = FixedCapacityArray<Element, InlineSize>()
  var overflowArray: [Element] = []
}

struct Matrix<Element, Colums: ConstantInt, Rows: ConstantInt> {
  struct StorageSize : ConstantInt {
    static var value: Int { Colums.value * Rows.value }
  }

  let storage: FixedSizeArray<Element, StorageSize>
}

@inline(never)
func printit(_ i: Int) {
  print(i)
}

@inline(never)
public func testLayout() {
  print(MemoryLayout<User>.size)
}

@inline(never)
public func testFixedSize() {
  let a = FixedSizeArray<Int, Const3>(repeating: 27)

  for x in a {
    printit(x)
  }
}

@inline(never)
public func testFixedCapacity() {
  var a = FixedCapacityArray<Int, Const3>()
  a.append(1)
  a.append(2)
  a.append(3)

  for x in a {
    printit(x)
  }
}

@main struct Main {
  static func main() {
    // CHECK:      layout:
    // CHECK-NEXT: 96 
    print("layout:")
    testLayout()

    // CHECK-NEXT: fixed size:
    // CHECK-NEXT: 27 
    // CHECK-NEXT: 27 
    // CHECK-NEXT: 27 
    print("fixed size:")
    testFixedSize()

    // CHECK-NEXT: fixed capacity:
    // CHECK-NEXT: 1 
    // CHECK-NEXT: 2 
    // CHECK-NEXT: 3 
    print("fixed capacity:")
    testFixedCapacity()
  }
}

// ===============================================================
// Stuff which should eventually go into the stdlib
// ===============================================================

@_rawLayout(likeArrayOf: Element, countDefinedBy: Size)
struct FixedSizeArray<Element, Size: ConstantInt> : Collection {
  init(repeating repeatedValue: Element) {
    initRawStorageSize(Size.self)
    withMutableAddressOfRawStorage(&self) {
      let p = $0.assumingMemoryBound(to: Element.self)
      p.initialize(repeating: repeatedValue, count: Size.value)
    }
  }

  var count: Int { Size.value }

  var startIndex: Int { 0 }
  var endIndex: Int { count }
  func index(after i: Int) -> Int { i + 1 }

  subscript(index: Int) -> Element {
    get {
      Self.checkSubscript(index)
      return withAddressOfRawStorage(self) {
        let p = $0.assumingMemoryBound(to: Element.self)
        return p[index] 
      }
    }
    set(rhs) {
      Self.checkSubscript(index)
      withMutableAddressOfRawStorage(&self) {
        let p = $0.assumingMemoryBound(to: Element.self)
        p[index] = rhs
      }
    }
  }


  private static func checkSubscript(_ index: Int) {
    precondition(index >= 0 && index < Size.value, "index out of bounds")
  }
}

struct FixedCapacityArray<Element, Size: ConstantInt> : Collection {

  @_rawLayout(likeArrayOf: Element, countDefinedBy: Size)
  private struct Storage {
    init() {
      initRawStorageSize(Size.self)
    }
  }

  private var storage = Storage()
  private(set) var count: Int

  init() {
    self.count = 0
  }

  static var capacity: Int { Size.value }

  var startIndex: Int { 0 }
  var endIndex: Int { count }
  func index(after i: Int) -> Int { i + 1 }

  subscript(index: Int) -> Element {
    get {
      checkSubscript(index)
      return withAddressOfRawStorage(storage) {
        let p = $0.assumingMemoryBound(to: Element.self)
        return p[index] 
      }
    }
    set(rhs) {
      checkSubscript(index)
      withMutableAddressOfRawStorage(&storage) {
        let p = $0.assumingMemoryBound(to: Element.self)
        p[index] = rhs
      }
    }
  }

  mutating func append(_ element: Element) {
    precondition(count < Self.capacity, "capacity overflow")
    withMutableAddressOfRawStorage(&storage) {
      let p = $0.assumingMemoryBound(to: Element.self)
      (p + count).initialize(to: element)
    }
    count += 1
  }


  private func checkSubscript(_ index: Int) {
    precondition(index >= 0 && index < count, "index out of bounds")
  }
}

