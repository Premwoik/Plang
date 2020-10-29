#include "ArrayList.h"
#include "Maybe.h"
namespace CoreNativeMaybe {}
namespace CoreMyFile {
using namespace CoreNativeMaybe;
String myFileFn();
String myFileFn() { return "ala"; }
} // namespace CoreMyFile
namespace CoreNativeList {
using namespace CoreMyFile;
}
namespace Main {
using namespace CoreNativeList;
class TestClass;
int Main();
class TestClass {
public:
  shared_ptr<ArrayList<int>> this___arr = nullptr;

public:
  TestClass() {}

public:
  TestClass(shared_ptr<ArrayList<int>> args___arr) { this___arr = args___arr; }
};
int Main() {
  shared_ptr<ArrayList<int>> arr1 = new ArrayList<int>();
  shared_ptr<ArrayList<int>> arr2 = new ArrayList<int>();
  TestClass tArr = TestClass(arr1);
  tArr.this___arr = arr2;
  return 0;
}
} // namespace Main
