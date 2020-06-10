#include "Core.h"
#include "MemoryInfo.h"
#include "ArrayList.h"
#include "unique_ptr.h"
#include "Arduino.h"
#include "ArrayList.h"
#include "Maybe.h"
namespace CoreNativeMaybe{
}
namespace CoreMyFile{
using namespace CoreNativeMaybe;
String myFileFn();
String myFileFn(){
   return "ala";
}
}
namespace CoreNativeList{
using namespace CoreMyFile;
}
namespace Core{
using namespace CoreNativeList;
void Void();
void printMemStats();
void Void(){
   }
int g___otherwise = 0;
void printMemStats(){
   float fragmentation = getFragmentation();
   Serial.print(F("Fragmentation level: "))   ;
   Serial.print(fragmentation)   ;
   Serial.println("%")   ;
}
class range{
public:
   int this___start = 0;
   int this___end = 0;
   int this___step = 1;
   range(int args___start, int args___end){
       this___start = args___start;
       this___end = args___end;
   }
   range(int args___start, int args___end, int args___step){
       this___start = args___start;
       this___end = args___end;
       this___step = args___step;
   }
   ArrayList<int> toList(){
      int len = (this___end - this___start);
      return ArrayList<int>();
   }
};
}
namespace Main{
using namespace Core;
int Main();
int Main(){
   Serial.print("String literal\n")   ;
   return 0;
}
}
