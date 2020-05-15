#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "Ethernet.h"
#include "Maybe.h"
#include "MemoryInfo.h"
#include "Message.h"
#include "MessageProcessor.h"
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
    this___start = args___start;
    this___end = args___end;
  }
  range(int args___start, int args___end, int args___step) {
    this___start = args___start;
    this___end = args___end;
    this___step = args___step;
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
    this___a = args___a;
    this___b = 100;
  }
  ThisTest(T args___a, int args___b) {
    this___a = args___a;
    this___b = 1;
  }
  T getA() { return this___a; }
  void setA(T args___val) { this___a = args___val; }
};
} // namespace Core
namespace CoreBoardUno {}
namespace CoreNativeEthernet {}
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
using namespace CoreBoardUno;
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState);
void initInputs(ArrayList<int> &args___ins);
void initEthernet();
int Main();
void loop();
Message processReadMsg(Message &args___msg);
EthernetServer g___server = EthernetServer(1000);
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState) {
  for (int i : args___out) {
    pinMode(i, OUTPUT);
    digitalWrite(i, args___initState);
  }
}
void initInputs(ArrayList<int> &args___ins) {
  for (int i : args___ins) {
    pinMode(i, INPUT);
  }
}
void initEthernet() {
  ArrayList<int> mac = ArrayList<int>(new int[6]{0, 1, 2, 3, 4, 5}, 6, 6);
  Ethernet.begin(mac.getNativePtr());
  Serial.println(Ethernet.localIP());
  g___server.begin();
}
int Main() {
  Serial.begin(9600);
  ArrayList<int> lowInitList = ArrayList<int>(new int[3]{13, 20, 21}, 3, 3);
  initOutputs(lowInitList, LOW);
  ArrayList<int> highInitList = ArrayList<int>(new int[3]{22, 23, 24}, 3, 3);
  initOutputs(highInitList, HIGH);
  ArrayList<int> inputsList = ArrayList<int>(new int[1]{30}, 1, 1);
  initInputs(inputsList);
  while (true) {
    loop();
  }
  return 0;
}
void loop() {
  EthernetClient client = g___server.available();
  while (client.connected() == 1) {
  }
  Serial.println("Client has connected!");
  MessageProcessor proc = MessageProcessor(client);
  while (client.connected() == 1) {
    shared_ptr<Message> msg = shared_ptr<Message>(proc.readMessage());
    if (true) {
      Message response = processReadMsg(msg);
      proc.sendMessage(response);
    }
  }
  Serial.println("Client has disconnected.");
  client.stop();
}
Message processReadMsg(Message &args___msg) { return Message(); }
