void printMemStats();
int mymax(int a, int b);
int Main();
template <typename K, typename V> class Pair {
public:
  K k;
  V v;
  Pair(K k, V v) {
    this->k = k;
    this->v = v;
  }
};
template <typename T> class Maybe {
public:
  T value;
  int hasValue = 0;
  Maybe() { this->hasValue = 0; }
  Maybe(T val) {
    this->hasValue = 1;
    this->value = val;
  }
};
#include "Arduino.h"
#include "ArrayList.h"
#include "MemoryInfo.h"
#include "unique_ptr.h"
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
template <typename T> class ThisTest {
public:
  T a;
  int b = 0;
  ThisTest(T a) {
    this->a = a;
    this->b = 100;
  }
  ThisTest(T a, int b) {
    this->a = a;
    this->b = b;
  }
  T getA() { return a; }
  void setA(T val) { this->a = val; }
};
int mymax(int a, int b) {
  int fuckT12;
  if (a > b) {
    fuckT12 = a;
  } else {
    fuckT12 = b;
  }
  return fuckT12;
}
int Main() {
  Serial.begin(9600);
  pinMode(13, OUTPUT);
  unique_ptr<ArrayList<int>> myList(new ArrayList<int>());
  int i = 0;
  while (true) {
    printMemStats();
    myList->add(10);
    if (i > 100) {
      myList->clear();
      Serial.println(F("Wyczyszczono liste!"));
    }
  }
  return 0;
}
