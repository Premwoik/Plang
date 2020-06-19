//
// Created by prw on 10.12.2019.
//

#ifndef MT_TEST_SHARED_PTR_H
#define MT_TEST_SHARED_PTR_H

class RC {
private:
    int count; // Reference count

public:
    void addRef() {
        // Increment the reference count
//        cout << "Increment the reference count" << endl;
        count++;
    }

    int release() {
        // Decrement the reference count and
        // return the reference count.
//        cout << "Decrement the reference count" << endl;
        return --count;
    }

    int status() {
        return count;
    }
};

template<typename T>
class shared_ptr {
private:
    T *pData; // Generic pointer to be stored
    RC *ref;

    void freeIfNeeded() {
        if (ref->release() == 0) {
            delete pData;
            delete ref;
        }
    }

public:
    shared_ptr(T *pValue) : pData(pValue), ref(0) {
        ref = new RC();
        ref->addRef();
    }

    shared_ptr (): pData(nullptr), ref(0){

    }

    shared_ptr(const shared_ptr<T> &sp) : pData(sp.pData), ref(sp.ref) {
        ref->addRef();
    }

    ~shared_ptr() {
        freeIfNeeded();
    }

    T &operator*() {
        return *pData;
    }

    T *operator->() {
        return pData;
    }

    int getRC() {
        return ref->status();
    }
    int size(){
        return sizeof(*ref);
    }

    bool isNotNull(){
        return pData != nullptr;
    }

    shared_ptr<T> &operator=(const shared_ptr<T> &r) {
        if (this != &r) {
            freeIfNeeded();
            pData = r.pData;
            ref = r.ref;
            ref->addRef();
        }
        return *this;
    }

    T* getNativePtr(){
        return pData;
    }
};

#endif //MT_TEST_SHARED_PTR_H
