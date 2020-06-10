//
// Created by prw on 12.10.2019.
//

#ifndef MT_TEST_MAYBE_H
#define MT_TEST_MAYBE_H


template<typename T>
class Maybe {
    typedef void (*func)(T);

    typedef void (*vfunc)();

private:
    T val{};
    bool hasValue = false;

public:

    Maybe() {};

    Maybe(T val) : val(val), hasValue(true) {};

    Maybe<T>* ifJust(func f);

    Maybe<T>* ifNothing(vfunc f);

    T fromMaybe(T def);

    bool isJust();

    bool isNothing();

    T get();
};

template<typename T>
T Maybe<T>::get() {
    return val;
}

template<typename T>
bool Maybe<T>::isNothing() {
    return !hasValue;
}

template<typename T>
bool Maybe<T>::isJust() {
    return hasValue;
}

template<typename T>
Maybe<T>* Maybe<T>::ifNothing(const vfunc f) {
    if (isNothing()) {
        f();
    }
    return this;
}

template<typename T>
Maybe<T>* Maybe<T>::ifJust(const func f) {
    if (isJust()) {
        f(val);
    }
    return this;
}

template<typename T>
T Maybe<T>::fromMaybe(T def) {
    if(isJust()) return val;
    return def;
}


#endif //MT_TEST_MAYBE_H
