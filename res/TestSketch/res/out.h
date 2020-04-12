#include "Arduino.h"
#include "ArrayList.h"
#include "Maybe.h"
#include "MemoryInfo.h"
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
template <typename T> class ThisTest {
public:
  T this___a;
  int this___b = 0;
  ThisTest() {}
  ThisTest(T args___a) {
    this___a = args___a;
    this___b = 100;
  }
  ThisTest(T args___a, int args___b) {
    this___a = args___a;
    this___b = args___b;
  }
  T getA() { return this___a; }
  void setA(T args___val) { this___a = args___val; }
};
} // namespace Core
using namespace Core;
using namespace CoreNativeList;
int myMax(int args___a, int args___b);
void testFunction(float args___c);
void funcThatReceiveClass();
void getNumber();
void funcThatReceiveClass(ThisTest<float> &args___c);
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
void testFunction(float args___c) {
  float b = g___b + 12.3;
  float c = args___c;
  float myC = g___b + args___c;
}
void funcThatReceiveClass() {}
void getNumber() {}
void funcThatReceiveClass(ThisTest<float> &args___c) {}
class MyClass {
public:
  MyClass() {}
  void getNumber() { ::getNumber(); }
};
int Main() {
  Serial.begin(9600);
  pinMode(13, OUTPUT);
  ArrayList<int> myList = ArrayList<int>();
  int i = 0;
  while (true) {
    delay(1000);
    printMemStats();
    myList.add(10);
    if ((i > 100) && (true)) {
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
  ArrayList<int> intList = ArrayList<int>(new int[1]{20}, 1, 1);
  ArrayList<shared_ptr<ThisTest<float>>> lTest =
      ArrayList<shared_ptr<ThisTest<float>>>();
  lTest.add(shared_ptr<ThisTest<float>>(new ThisTest<float>(10.1, 100)));
  lTest.get(1);
  Serial.print(CoreMyFile::myFileFn());
  return 0;
}
