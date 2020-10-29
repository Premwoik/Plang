//
// Created by prw on 25.03.2020.
//

#ifndef MT_TEST_LIST_H
#define MT_TEST_LIST_H

#include "Maybe.h"
#include "unique_ptr.h"

template<typename T>
class List {

public:
    virtual T get(int index) = 0;

    virtual uint8_t replace(int index, T elem) = 0;

    virtual Maybe<T> removeLast() = 0;

    virtual void add(T elem) = 0;

    virtual void add(int pos, T elem) = 0;

    virtual void addAll(List<T> &list) {
        for (int i = 0; i < list.size(); i++) {
            add(list.get(i));
        }
    }

    virtual void clear() = 0;

    virtual Maybe<T> safeRemove(int pos) = 0;

    virtual T remove(int pos) = 0;

    virtual unique_ptr<List<T>> subList(int start, int end) = 0;

    virtual int size() = 0;

    virtual int sizeInBytes() = 0;

};

template<typename T>
class ListValueGetter {
public:
    virtual T getValue(int index) = 0;

    virtual Maybe<T> getSafeValue(int index) = 0;
};

#endif //MT_TEST_LIST_H
