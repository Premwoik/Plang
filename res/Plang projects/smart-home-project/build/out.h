#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "DallasTemperature.h"
#include "Ethernet.h"
#include "Maybe.h"
#include "MemoryInfo.h"
#include "OneWire.h"
#include "SPI.h"
#include "Wire.h"
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
namespace SmartHomeNativeOneWire {}
namespace CoreNativeEthernet {}
namespace SmartHomeMessage {
using namespace CoreNativeList;
using namespace Core;
class Message;
Message okMsg(uint8_t args___code);
Message errorMsg(uint8_t args___code);
class Message {
private:
  int this___code = 0;

private:
  int this___status = 0;

private:
  ArrayList<uint8_t> this___args = ArrayList<uint8_t>();

public:
  bool this___valid = false;

public:
  Message(uint8_t args___code, uint8_t args___status,
          ArrayList<uint8_t> &args___args) {
    this___code = args___code;
    this___status = args___status;
  }

public:
  Message(uint8_t args___code, uint8_t args___status) {
    this___code = args___code;
    this___status = args___status;
  }

public:
  Message(ArrayList<uint8_t> &args___data, int args___s) {
    this___code = args___data.get(0);
    args___data.remove(0);
    this___args = args___data;
  }

public:
  ArrayList<uint8_t> toBytes() {
    ArrayList<uint8_t> bytes =
        ArrayList<uint8_t>(new uint8_t[2]{255, this___code}, 2, 2);
    if (this___status > 0) {
      bytes.add(this___status);
    }
    for (uint8_t a : this___args) {
      bytes.add(a);
    }
    bytes.add(250);
    return bytes;
  }

public:
  int bytesLength() {
    if (this___status > 0) {
      return this___args.size() + 4;
    } else {
      return this___args.size() + 3;
    }
  }

public:
  void print() {}

public:
  int getCode() { return this___code; }

public:
  int getLength() { return this___args.size(); }

public:
  uint8_t getArg(int args___i) { return this___args.get(args___i); }
};
Message okMsg(uint8_t args___code) { return Message(args___code, 200); }
Message errorMsg(uint8_t args___code) { return Message(args___code, 40); }
} // namespace SmartHomeMessage
namespace SmartHomeNativeDallasTemperature {
using namespace SmartHomeNativeOneWire;
}
namespace SmartHomeInitializers {
using namespace Core;
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState);
void initInputs(ArrayList<int> &args___ins);
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
} // namespace SmartHomeInitializers
namespace SmartHomeInterrupts {
using namespace Core;
using namespace CoreNativeList;
class EnergyMeter;
void initMeters(ArrayList<int> &args___pins);
void listenerDef(EnergyMeter &args___meter);
void listener0();
void listener1();
void listener2();
void listener3();
void listener4();
void listener5();
void listener6();
ArrayList<EnergyMeter> g___energyMeters = ArrayList<EnergyMeter>();
class EnergyMeter {
public:
  int this___counter = 0;

public:
  int this___pin = 0;

public:
  int this___lastRead = 0;

public:
  EnergyMeter() {}

public:
  EnergyMeter(int args___pin) {
    this___pin = args___pin;
    pinMode(args___pin, INPUT_PULLUP);
  }

public:
  void clear() { int counter = 0; }
};
void initMeters(ArrayList<int> &args___pins) {
  for (int i = 0; i < args___pins.size(); i += 1) {
    int pin = digitalPinToInterrupt(args___pins.get(i));
    if (i == 0) {
      attachInterrupt(pin, listener0, RISING);
    } else if (i == 1) {
      attachInterrupt(pin, listener1, RISING);
    } else if (i == 2) {
      attachInterrupt(pin, listener2, RISING);
    } else if (i == 3) {
      attachInterrupt(pin, listener3, RISING);
    } else if (i == 4) {
      attachInterrupt(pin, listener4, RISING);
    } else if (i == 5) {
      attachInterrupt(pin, listener5, RISING);
    } else {
      attachInterrupt(pin, listener6, RISING);
    }
    g___energyMeters.add(EnergyMeter(args___pins.get(i)));
  }
}
void listenerDef(EnergyMeter &args___meter) {
  uint32_t currentRead = millis();
  if (currentRead - args___meter.this___lastRead > 10) {
    Serial.print("Impuls: ");
    Serial.println(args___meter.this___pin);
    args___meter;
    args___meter;
  }
}
void listener0() {
  EnergyMeter meter = g___energyMeters.get(0);
  listenerDef(meter);
}
void listener1() {
  EnergyMeter meter = g___energyMeters.get(1);
  listenerDef(meter);
}
void listener2() {
  EnergyMeter meter = g___energyMeters.get(2);
  listenerDef(meter);
}
void listener3() {
  EnergyMeter meter = g___energyMeters.get(3);
  listenerDef(meter);
}
void listener4() {
  EnergyMeter meter = g___energyMeters.get(4);
  listenerDef(meter);
}
void listener5() {
  EnergyMeter meter = g___energyMeters.get(5);
  listenerDef(meter);
}
void listener6() {
  EnergyMeter meter = g___energyMeters.get(6);
  listenerDef(meter);
}
} // namespace SmartHomeInterrupts
namespace SmartHomeMessageProcessor {
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
using namespace SmartHomeMessage;
class MessageProcessor;
class MessageProcessor {
private:
  EthernetClient this___client;

public:
  MessageProcessor(EthernetClient &args___client) {
    this___client = args___client;
  }

public:
  shared_ptr<Message> readMessage() {
    if (this___client.available() > 0) {
      Serial.println("Start recaiving message...");
      ArrayList<uint8_t> tmp = ArrayList<uint8_t>();
      bool isMessage = false;
      while (this___client.available() > 0) {
        uint8_t readByte = this___client.read();
        if (isMessage) {
          if (readByte == 250) {
            return new Message(tmp, tmp.size());
          }
          tmp.add(readByte);
        } else {
          if (readByte == 255) {
            isMessage = true;
          }
        }
      }
    }
    return nullptr;
  }

public:
  void sendMessage(Message &args___message) {
    Serial.println("Sending response...");
    args___message.print();
    ArrayList<uint8_t> bytes = args___message.toBytes();
    this___client.write(bytes.getNativePtr(), args___message.bytesLength());
  }
};
} // namespace SmartHomeMessageProcessor
namespace SmartHomeMsgCode {
int g___Test = 200;
int g___SetLow = 100;
int g___SetHigh = 101;
int g___SetPwm = 105;
int g___SetTimeDimmer = 102;
int g___ReadTemp = 110;
int g___ReadEnergy = 111;
} // namespace SmartHomeMsgCode
namespace SmartHomeMsgStatus {
int g___WrongCode = 50;
int g___Ok = 200;
} // namespace SmartHomeMsgStatus
namespace SmartHomeThermometers {
using namespace Core;
using namespace CoreNativeList;
using namespace SmartHomeNativeDallasTemperature;
using namespace SmartHomeNativeOneWire;
class DallasThermometers;
class DallasThermometers {
public:
  DallasTemperature this___dt;

public:
  DallasThermometers() {}

public:
  void setLinePrecision(int args___precision) {
    int num = this___dt.getDeviceCount();
    ArrayList<uint8_t> deviceAddr =
        ArrayList<uint8_t>(new uint8_t[8]{0, 0, 0, 0, 0, 0, 0, 0}, 8, 8);
    for (int id = 0; id < num; id += 1) {
      this___dt.getAddress(deviceAddr.getNativePtr(), id);
      this___dt.setResolution(deviceAddr.getNativePtr(), args___precision);
    }
  }

public:
  DallasThermometers(OneWire &args___line) {
    this___dt = DallasTemperature(&args___line);
    this___dt.begin();
    setLinePrecision(9);
  }

public:
  ArrayList<uint8_t> readTemps() {
    int num = this___dt.getDeviceCount();
    ArrayList<uint8_t> result = ArrayList<uint8_t>();
    if (num > 0) {
      this___dt.requestTemperatures();
      for (int i = 0; i < num; i += 1) {
        ArrayList<uint8_t> addr =
            ArrayList<uint8_t>(new uint8_t[8]{0, 0, 0, 0, 0, 0, 0, 0}, 8, 8);
        this___dt.getAddress(addr.getNativePtr(), i);
        int raw = this___dt.getTemp(addr.getNativePtr());
        for (uint8_t j : addr) {
          result.add(j);
        }
        result.add(highByte(raw));
        result.add(lowByte(raw));
      }
    }
    return result;
  }
};
} // namespace SmartHomeThermometers
namespace SmartHomeTimer {
using namespace Core;
using namespace CoreNativeList;
class Task;
void addTask(nonstd::function<void(int)> args___fun, int args___pin,
             int args___timeout);
void runTasks();
class Task {
public:
  nonstd::function<void(int)> this___fun;

public:
  uint32_t this___time;

public:
  int this___pin;

public:
  Task() {}

public:
  Task(nonstd::function<void(int)> args___fun, int args___pin,
       uint32_t args___time) {
    this___pin = args___pin;
    this___fun = args___fun;
    this___time = args___time;
  }

public:
  void runFun() { this___fun(this___pin); }

public:
  uint32_t getTime() { return this___time; }
};
ArrayList<Task> g___tasks = ArrayList<Task>();
void addTask(nonstd::function<void(int)> args___fun, int args___pin,
             int args___timeout) {
  g___tasks.add(Task(args___fun, args___pin, args___timeout));
}
void runTasks() {
  if (g___tasks.size() == 0) {
    return Void();
  }
  ArrayList<int> finished = ArrayList<int>();
  uint32_t time = millis();
  int i = 0;
  for (Task t : g___tasks) {
    if (t.getTime() < time) {
      t.runFun();
      finished.add(i);
    }
    i = i + 1;
  }
  for (int i : finished) {
    g___tasks.remove(i);
  }
}
} // namespace SmartHomeTimer
namespace Main {
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
using namespace SmartHomeMessage;
using namespace SmartHomeMessageProcessor;
using namespace SmartHomeInitializers;
using namespace SmartHomeInterrupts;
using namespace SmartHomeThermometers;
using namespace SmartHomeNativeOneWire;
void initEthernet(ArrayList<uint8_t> &args___mac);
void setupPins();
int Main();
void loop();
bool stillAlive();
Message processReadMsg(Message &args___msg);
Message digitalWriteCmd(Message &args___msg);
Message setTimeDimmerCmd(Message &args___msg);
Message readEnergyCmd(Message &args___msg);
Message readTempCmd(Message &args___msg);
EthernetServer g___server = EthernetServer(1000);
uint32_t g___timeout = 60000;
uint32_t g___lastReadMessageTime = 0;
OneWire g___line2 = OneWire(5);
DallasThermometers g___tempLine = DallasThermometers(g___line2);
void initEthernet(ArrayList<uint8_t> &args___mac) {
  Serial.println(F("Initializing connection with dns server:"));
  while (Ethernet.begin(args___mac.getNativePtr()) == 0) {
    Serial.println(F("Failure! Reconnectiong..."));
    delay(1000);
  }
  Serial.print(F("Success! Connected with ip: "));
  Serial.println(Ethernet.localIP());
  g___server.begin();
}
void setupPins() {
  ArrayList<int> lowInitList = ArrayList<int>();
  ArrayList<int> highInitList =
      ArrayList<int>(new int[32]{18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
                                 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                                 40, 41, 42, 43, 44, 45, 46, 47, 48, 49},
                     32, 32);
  initOutputs(highInitList, HIGH);
  ArrayList<int> energyPins = ArrayList<int>(new int[1]{2}, 1, 1);
  initMeters(energyPins);
}
int Main() {
  Serial.begin(9600);
  setupPins();
  Serial.println(F("Hello"));
  ArrayList<uint8_t> mac =
      ArrayList<uint8_t>(new uint8_t[6]{0, 1, 2, 3, 4, 5}, 6, 6);
  initEthernet(mac);
  while (true) {
    SmartHomeTimer::runTasks();
    loop();
  }
  return 0;
}
void loop() {
  EthernetClient client = g___server.available();
  while (!client) {
    return Void();
  }
  Serial.println(F("Client has connected!"));
  MessageProcessor proc = MessageProcessor(client);
  g___lastReadMessageTime = millis();
  while ((stillAlive()) && (client.connected())) {
    SmartHomeTimer::runTasks();
    shared_ptr<Message> msg = proc.readMessage();
    if (msg.isNotNull()) {
      Message response = processReadMsg(*msg);
      proc.sendMessage(response);
      g___lastReadMessageTime = millis();
      Serial.println(F("--------"));
      printMemStats();
    }
  }
  Serial.println(F("Client has disconnected."));
  client.stop();
}
bool stillAlive() {
  uint32_t time = millis();
  return (g___lastReadMessageTime + g___timeout) > time;
}
Message processReadMsg(Message &args___msg) {
  if (args___msg.getCode() == SmartHomeMsgCode::g___SetHigh) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == SmartHomeMsgCode::g___SetLow) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == SmartHomeMsgCode::g___SetTimeDimmer) {
    return setTimeDimmerCmd(args___msg);
  } else if (args___msg.getCode() == SmartHomeMsgCode::g___Test) {
    Serial.println(F("Test message!"));
    return okMsg(SmartHomeMsgCode::g___Test);
  } else if (args___msg.getCode() == SmartHomeMsgCode::g___ReadEnergy) {
    Serial.println(F("Read energy message!"));
    return readEnergyCmd(args___msg);
  } else if (args___msg.getCode() == SmartHomeMsgCode::g___ReadTemp) {
    Serial.println(F("Read temp message!"));
    return readTempCmd(args___msg);
  } else {
    return errorMsg(args___msg.getCode());
  }
}
Message digitalWriteCmd(Message &args___msg) {
  Serial.println(F("DigitalWrite message"));
  for (int i = 0; i < args___msg.getLength(); i += 2) {
    digitalWrite(args___msg.getArg(i), args___msg.getArg(i + 1));
  }
  return okMsg(args___msg.getCode());
}
Message setTimeDimmerCmd(Message &args___msg) {
  Serial.println(F("SetTimeDimmer message"));
  for (int i = 0; i < args___msg.getLength(); i += 3) {
    uint8_t pin = args___msg.getArg(i);
    uint8_t isInverted = args___msg.getArg(i + 1);
    uint32_t timeout = millis() + args___msg.getArg(i + 2) * 100;
    if (isInverted) {
      digitalWrite(pin, HIGH);
      auto fun = [&](int args___x) {
        digitalWrite(args___x, LOW);
        Serial.println(F("Postponed task dimmer"));
        Serial.print(pin);
      };
      SmartHomeTimer::addTask(fun, pin, timeout);
    } else {
      digitalWrite(pin, LOW);
      auto fun = [&](int args___x) { return digitalWrite(args___x, HIGH); };
      SmartHomeTimer::addTask(fun, pin, timeout);
    }
  }
  return okMsg(args___msg.getCode());
}
Message readEnergyCmd(Message &args___msg) {
  ArrayList<uint8_t> args = ArrayList<uint8_t>();
  for (EnergyMeter meter : g___energyMeters) {
    args.add(meter.this___pin);
    args.add(meter.this___counter);
    meter.clear();
  }
  return Message(args___msg.getCode(), 200, args);
}
Message readTempCmd(Message &args___msg) {
  ArrayList<uint8_t> res = g___tempLine.readTemps();
  return Message(args___msg.getCode(), 200, res);
}
} // namespace Main
