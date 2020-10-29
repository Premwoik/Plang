//
// Created by prw on 10.03.18.
//

#include "Message.h"

Message::Message(uint8_t code, uint8_t status, uint8_t *args) : code(code), status(status){
    if (args && *args > 0){
    	this->argsLength = *args;
        this->args = args + 1;
//        for(int i=0; i< *args; i++){
//        	this->args[i] = *(args + 1 + i);
//        }
//        delete [] args;
    }
    else{
    	if(args) delete [] args;
        this->args = nullptr;
        this->argsLength = 0;
    }
}


Message::Message(uint8_t* data, uint8_t length) {
	argsLength = length-1;
	code = data[0];
	auto args = new uint8_t[length];
	this->args = args + 1;
	for (int i = 1; i < length; ++i) {
		this->args[i-1] = data [i];
	}
}



Message::~Message() {
	if(args != nullptr){
		delete[] (args - 1);
		args = nullptr;
	}
}

bool Message::validate(){
	switch(code){
	case READ_INPUTS:
	case READ_POWER:
	case READ_TERM:
	case SET_HIGH:
	case SET_LOW:
	case SET_PWM:
	case INPUTS_RAPORT:
	case TEST:
		valid = true;
		return true;
	default:
		valid = false;
		return false;
	}
}

void Message::setArgs(uint8_t *args) {
    if (this->args != nullptr && args != nullptr)
        for (int i = 0; i < argsLength; ++i)
            this->args[i] = args[i];
}

void Message::setArg(uint8_t index, uint8_t byte) {
    if (index < argsLength && args != NULL)
        this->args[index] = byte;
}

//check this function
uint8_t* Message::toBytes() {
	auto index = 0;
	auto length = bytesLength();

    auto resp = new uint8_t [length];
    resp[index++] = 255;
    resp[index++] = code;
    if(status > 0){
    	resp[index++] = status;
    }
    for (int i = 0; i < argsLength; ++i) {
		resp[index++] = this->args[i];
	}
    resp[index] = 250;
    return resp;
}

uint8_t Message::bytesLength() const{
	return status > 0 ? argsLength + 4 : argsLength + 3;
}


void Message::print() const{
  Serial.print("CODE: "); Serial.println(code);
  Serial.print("STATUS: "); Serial.println(status);
  Serial.print("ARGS: ");
  for (int i = 0; i < argsLength; ++i) {
	Serial.print(args[i]);
	Serial.print(";");
  }
  Serial.println("");
  Serial.print("VALID: "); Serial.println(valid);
}


uint8_t Message::getArg(int index) const {
    return args[index];
}



Message Message::okMsg(uint8_t code){
	return Message(code, Ok, 0);
}

Message Message::errorMsg(uint8_t code, uint8_t error = Error){
	return Message(code, error);
}


