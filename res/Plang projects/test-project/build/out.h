#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "Maybe.h"
#include "MemoryInfo.h"
namespace CoreNativeMaybe {}
namespace CoreMyFile {
using namespace CoreNativeMaybe;
String myFileFn();
String myFileFn() { return "ala"; }
} // namespace CoreMyFile
namespace CoreNativeList {
using namespace CoreMyFile;
}
namespace Core {
using namespace CoreNativeList;
void Void();
void printMemStats();
class range;
void Void() {}
int g___otherwise = 0;
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
class range {
public:
  int this___start = 0;

public:
  int this___end = 0;

public:
  int this___step = 1;

public:
  range(int args___start, int args___end) {
    this___start = args___start;
    this___end = args___end;
  }

public:
  range(int args___start, int args___end, int args___step) {
    this___start = args___start;
    this___end = args___end;
    this___step = args___step;
  }

public:
  ArrayList<int> toList() {
    int len = (this___end - this___start);
    return ArrayList<int>();
  }
};
} // namespace Core
namespace Main {
using namespace Core;
using namespace CoreNativeList;
class TestClass;
int Main();
class TestClass {
public:
  shared_ptr<ArrayList<int>> this___arr;

public:
  TestClass() {}

public:
  TestClass(shared_ptr<ArrayList<int>> args___arr) { this___arr = args___arr; }
};
int Main() {
  ArrayList<int> arr = ArrayList<int>();
  arr.add(12);
  TestClass tArr = TestClass(&arr);
  shared_ptr<ArrayList<int>> arrPtr = tArr.this___arr;
  if (arrPtr.isNotNull()) {
    for (int i : arr) {
      Serial.println(i);
    }
  }
  return 0;
}
} // namespace Main
