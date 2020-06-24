//
// Created by prw on 10.10.2019.
//

#ifndef MT_TEST_ARRAYLIST_H
#define MT_TEST_ARRAYLIST_H

#include "List.h"
#include "Maybe.h"
//using namespace std;


template<typename T>
class ArrayListBase : public List<T> {
#define INIT_ARRAY_SIZE 10
#define ARRAY_GROW 5
private:
    T *arr{};

protected:
    int accSize{};
    int maxSize{};

    ArrayListBase() : List<T>() {
        initialize();
    }

    ArrayListBase(T *arr, int size, int maxSize) : arr(arr), accSize(size), maxSize(maxSize), List<T>() {}

    ~ArrayListBase() {
        delete[] arr;
    }

    void initialize() {
        this->arr = new T[INIT_ARRAY_SIZE]{};
        this->accSize = 0;
        this->maxSize = INIT_ARRAY_SIZE;
    }
    typedef T* iterator;
    typedef const T* const_iterator;

public:

    // A simplistic implementation of operator= (see better implementation below)
    ArrayListBase<T>& operator= (const ArrayListBase<T> &rhs)
    {
        if(this->arr != nullptr) {
            this->clear();
        }
        if(rhs.arr != nullptr){
            for(auto e : rhs){
                this->add(e);
            }
        }
        return *this;
    }

    iterator begin() { return &arr[0]; }
    const_iterator begin() const { return &arr[0]; }
    iterator end() { return &arr[accSize]; }
    const_iterator end() const { return &arr[accSize]; }

    T* getNativePtr(){
        return arr;
    }

    T get(int index) {
        return arr[index];
    }

    void set(T elem, int index){
        replace(index, elem);
    }

    uint8_t replace(int index, T elem) {
        if (index < accSize) {
            arr[index] = elem;
            return 0;
        } else {
            return 1;
        }
    }

    Maybe<T> removeLast() {
        if (accSize > 0) {
            accSize--;
            return Maybe<T>(arr[accSize]);
        } else return Maybe<T>();
    }

    void add(T elem) {
        accSize += 1;
        growArray();
        arr[accSize - 1] = elem;
    }

    void add(int pos, T elem) {
        accSize += 1;
        growArray();
        copyArray(arr, pos, arr, pos + 1, accSize - pos - 2);
    }

    void clear() {
        this->~ArrayListBase();
        this->arr = new T[INIT_ARRAY_SIZE]{};
        this->accSize = 0;
        this->maxSize = INIT_ARRAY_SIZE;
    }

    T remove(int pos) {
        auto tmp = get(pos);
        accSize -= 1;
        copyArray(arr, pos + 1, arr, pos, accSize - pos);
//        TODO add decreasing array size when too many indexes are free
// Maybe while allocating or deallocating memory actual values should be stored on the stack instead of heap
// It will be slowed, but sometimes it can protect the memory from the defragmentation :O
// Although if array contains pointers to objects, the memory addresses will be probably used after the list and the gap will exist after all :<
        return tmp;
    }

    Maybe<T> safeRemove(int pos) {
        if (pos < accSize) {
            auto tmp = remove(pos);
            return Maybe<T>(tmp);
        }
        return Maybe<T>();
    }

    virtual List<T>* newList(T *arr, int size, int maxSize){
        return nullptr;
    }

    unique_ptr<List<T>> subList(int start, int end) override {
        auto len = end - start + ARRAY_GROW;
        auto nsize = end - start;
        auto n = new T[len];
        copyArray(arr, start, n, 0, nsize);
        return unique_ptr<List<T>>(newList(n, nsize, len));
    }

    int size() {
        return accSize;
    }

    int sizeInBytes() {
        return sizeof(T) * maxSize;
    }

private:

    void growArray() {
        if (accSize == maxSize) {
            maxSize += ARRAY_GROW;
            T *tmp = new T[maxSize];
            copyArray(arr, 0, tmp, 0, accSize);
            delete[] arr;
            arr = tmp;
        }
    }

    void copyArray(T *oldArr, int sourcePos, T *newArr, int destPos, int len) {
        for (int i = 0; i < len; i++) {
            newArr[destPos + i] = oldArr[sourcePos + i];
        }
    }
};


template<typename T>
class ArrayList : public ArrayListBase<T>, ListValueGetter<T> {

public:
    ArrayList() : ArrayListBase<T>() {}

    ArrayList(T *arr, int size, int maxSize) : ArrayListBase<T>(arr, size, maxSize) {}

    T getValue(int index) {
        return this->get(index);
    }


    List<T> *newList(T *arr, int size, int maxSize){
        return new ArrayList(arr, size, maxSize);
    }

    Maybe<T> getSafeValue(int index) override {
        if (index < this->accSize) {
            return Maybe<T>(getValue(index));
        }
        return Maybe<T>();
    }

private:
};

template<typename T>
class ArrayList<T *> : public ArrayListBase<T *>, ListValueGetter<T> {

public:
    ArrayList() : ArrayListBase<T *>() {}

    ArrayList(T **arr, int size, int maxSize) : ArrayListBase<T *>(arr, size, maxSize) {}

    ~ArrayList() {
        for (auto i = 0; i < this->maxSize; i++) {
            auto obj = this->get(i);
            if (obj != nullptr) delete (obj);
        }
    }

    List<T> *newList(T *arr, int size, int maxSize)  {
        return new ArrayList(arr, size, maxSize);
    }


//    T& set(int index){
//        return *getValue(index);
//    }

    void clear() override {
        this->~ArrayList();
        this->initialize();
    }

    T getValue(int index) {
        return *this->get(index);
    }

    Maybe<T> getSafeValue(int index) override {
        if (index < this->accSize) {
            auto res = this->get(index);
            if (res != nullptr) {
                return *res;
            }
        }
        return Maybe<T>();
    }

};


#endif //MT_TEST_ARRAYLIST_H
