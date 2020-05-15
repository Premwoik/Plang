#ifndef HOME_BETTER_MESSAGE_H
#define HOME_BETTER_MESSAGE_H

#include<Arduino.h>



class Message {
private:
	uint8_t code = 0;
	uint8_t status = 0;
	uint8_t argsLength = 0;
	uint8_t *args = nullptr;


public:
	enum CODE {TEST = 200, SET_LOW = 100, SET_HIGH = 101, SET_PWM = 105, READ_TERM = 110, READ_POWER = 111, READ_INPUTS = 112, INPUTS_RAPORT = 150, ACTIVE_INPUTS = 151, UNKNOWN = 0};
	enum STATUS {Ok = 200, Error = 40, WrongCode = 50, NotValid = 60, ArgsMissing = 70};

	bool valid = false;

	Message(uint8_t code, uint8_t status, uint8_t *args = nullptr);

	Message(uint8_t* data, uint8_t length);


	Message();

	~Message();

	void setArgs(uint8_t *args);

	void setArg(uint8_t index, uint8_t byte);

	uint8_t getArg(int index) const;

	uint8_t getLength() const{
		return argsLength;
	}

	uint8_t getCode() const{
		return code;
	}

	uint8_t *toBytes();

	uint8_t bytesLength() const;

	void print() const;

	bool validate();


	static Message okMsg(uint8_t code);
	static Message errorMsg(uint8_t code, uint8_t error);

};

#endif //HOME_BETTER_MESSAGE_H

