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
    T val;
    bool isValue = false;

public:

    Maybe() {};

    Maybe(T val) : val(val) {};

    auto ifJust(func f);

    auto ifNothing(vfunc f);

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
    return 0;
}

template<typename T>
bool Maybe<T>::isJust() {
    return 0;
}

template<typename T>
auto Maybe<T>::ifNothing(const vfunc f) {
    if (!isValue) {
        f();
    }
    return this;
}

template<typename T>
auto Maybe<T>::ifJust(const func f) {
    if (isValue) {
        f(val);
    }
    return this;
}


#endif //MT_TEST_MAYBE_H
