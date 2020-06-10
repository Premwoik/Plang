//
// Created by prw on 10.12.2019.
//

#ifndef MT_TEST_UNIQUE_PTR_H
#define MT_TEST_UNIQUE_PTR_H
template<typename T>
class unique_ptr {
private:
    T *pData; // Generic pointer to be stored

public:
    unique_ptr(T *pValue) : pData(pValue) {
    }

    ~unique_ptr() {
        delete pData;
    }

    T &operator*() {
        return *pData;
    }

    T *operator->() {
        return pData;
    }
};
#endif //MT_TEST_UNIQUE_PTR_H
