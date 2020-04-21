#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "Ethernet.h"
#include "Maybe.h"
#include "MemoryInfo.h"
#include "SPI.h"
#include "unique_ptr.h"
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
void printMemStats();
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
class range {
public:
  int this___start = 0;
  int this___end = 0;
  int this___step = 1;
  range(int args___start, int args___end) {
    this___start;
    this___end;
  }
  range(int args___start, int args___end, int args___step) {
    this___start;
    this___end;
    this___step;
  }
  ArrayList<int> toList() {
    int len = (this___end - this___start);
    return ArrayList<int>();
  }
};
template <typename T> class ThisTest {
public:
  T this___a;
  int this___b = 0;
  ThisTest() {}
  ThisTest(T args___a) {
    this___a;
    this___b;
  }
  ThisTest(T args___a, int args___b) {
    this___a;
    this___b;
  }
  T getA() { return this___a; }
  void setA(T args___val) { this___a; }
};
} // namespace Core
namespace CoreNativeEthernet {}
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
int myMax(int args___a, int args___b);
void testFunction(float &args___c);
void funcThatReceiveClass();
void getNumber();
void funcThatReceiveClass(ThisTest<float> args___c);
shared_ptr<ThisTest<float>> fThatReturnPtr();
int Main();
int myMax(int args___a, int args___b) {
  int var1;
  if (args___a > args___b) {
    var1 = args___a;
  } else {
    var1 = args___b;
  }
  return var1;
}
float g___b = 12.3;
ThisTest<int> g___c = ThisTest<int>(10, 10);
void testFunction(float &args___c) {
  float b = g___b + 12.3;
  float &c = args___c;
  c = 12.0;
  float myC = g___b + args___c;
}
void funcThatReceiveClass() {}
void getNumber() {}
void funcThatReceiveClass(ThisTest<float> args___c) {}
class MyClass {
public:
  MyClass() {}
  void getNumber() { ::getNumber(); }
};
shared_ptr<ThisTest<float>> fThatReturnPtr() {
  return new ThisTest<float>(10.1, 100);
}
int Main() {
  Serial.begin(9600);
  pinMode(13, OUTPUT);
  ArrayList<int> myList = ArrayList<int>();
  int i = 0;
  while (true) {
    delay(1000);
    printMemStats();
    myList.add(10);
    if ((i > 100) && ((true) || (true))) {
      myList.clear();
      Serial.println(F("Wyczyszczono liste!"));
    }
    ThisTest<float> classTest = ThisTest<float>(13.0, 100);
    float z = classTest.getA();
    Serial.println(z);
    i = i + 20;
    Serial.println(classTest.getA());
    Serial.println(i);
    ThisTest<float> bClass = classTest;
    bClass = classTest;
    bClass = classTest;
    bClass = classTest;
    Serial.println("sees");
    Serial.print("HEHEH");
    ArrayList<shared_ptr<ThisTest<float>>> mList =
        ArrayList<shared_ptr<ThisTest<float>>>(
            new shared_ptr<ThisTest<float>>[2] {
              shared_ptr<ThisTest<float>>(new ThisTest<float>(13.0, 100)),
                  shared_ptr<ThisTest<float>>(new ThisTest<float>(13.0, 100))
            },
            2, 2);
    shared_ptr<ThisTest<float>> result = mList.get(1);
    funcThatReceiveClass(classTest);
  }
  shared_ptr<ThisTest<float>> a1 = new ThisTest<float>(13.0, 100);
  ThisTest<float> a2 = *a1;
  shared_ptr<ThisTest<float>> a3 = a1;
  shared_ptr<ThisTest<float>> a4 = a1;
  shared_ptr<ThisTest<float>> a5 = a1;
  ArrayList<int> intList = ArrayList<int>(new int[1]{20}, 1, 1);
  intList.get(0);
  intList.set(10, 1);
  ArrayList<shared_ptr<ThisTest<float>>> lTest =
      ArrayList<shared_ptr<ThisTest<float>>>();
  lTest.add(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.1, 100)));
  lTest.add(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.1, 100)));
  lTest.add(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.1, 100)));
  lTest.add(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.1, 100)));
  lTest.get(1);
  lTest.set(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.2, 101)), 1);
  Serial.print(CoreMyFile::myFileFn());
  float c = ((10.0 + 11.0) * 1.0 / 100.0) / 10.0;
  testFunction(c);
  Serial.print(c);
  ArrayList<int> intPtr = ArrayList<int>(new int[1]{10}, 1, 1);
  needPointer(intPtr.getNativePtr());
  shared_ptr<int> gotPtr = shared_ptr<int>(getPointer());
  bool d = true;
  ThisTest<float> xd = ThisTest<float>(10.1, 100);
  shared_ptr<ThisTest<float>> x2 = new ThisTest<float>{xd};
  ThisTest<float> haha = ThisTest<float>(10.1, 1000);
  return 0;
}
