//
// Created by prw on 12.05.2020.
//

#ifndef MT_TEST_BITWISE_H
#define MT_TEST_BITWISE_H


inline void setBitHigh(volatile uint8_t * reg, uint8_t number){
    *reg |= (1 << number);
}


inline void setBitLow(volatile uint8_t * reg, uint8_t number){
    *reg &= ~(1 << number);
}

inline void setAllLow(volatile uint8_t * reg){
    *reg = 0b00000000;
}


#endif //MT_TEST_BITWISE_H
