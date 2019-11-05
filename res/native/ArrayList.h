//
// Created by prw on 10.10.2019.
//

#ifndef MT_TEST_ARRAYLIST_H
#define MT_TEST_ARRAYLIST_H

using namespace std;

#include <string>

template<typename T>
class ArrayList {
#define INIT_ARRAY_SIZE 10
#define ARRAY_GROW 5
private:
    T *arr;
    int accSize;
    int maxSize;


public:

    ArrayList() {
        this->arr = new T[INIT_ARRAY_SIZE];
        this->accSize = 0;
        this->maxSize = INIT_ARRAY_SIZE;
    }

    ArrayList(T *arr, int size, int maxSize): arr(arr), accSize(size), maxSize(maxSize){}

    virtual ~ArrayList() {

        delete[] arr;
    }

    T get(int index) {
        return arr[index];
    }

    int replace(int index, T elem) {
        if (index < accSize) {
            arr[index] = elem;
            return 0;
        } else {
            return 1;
        }
    }

    T removeLast() {
        if (accSize > 0) {
            accSize--;
            return arr[accSize];
        } else throw string("list is actually empty!");
    }

    void add(T elem) {
        accSize += 1;
        growArray();
        arr[accSize - 1] = elem;
    }

    void add(int pos, T elem){
        accSize += 1;
        growArray();
        copyArray(arr, pos, arr, pos + 1, accSize - pos - 2);
    }

    T remove(int pos){
        auto tmp = get(pos);
        accSize -= 1;
        copyArray(arr, pos + 1, arr, pos, accSize - pos);
//        TODO add decreasing array size when too many indexes are free
        return tmp;
    }

    ArrayList<T>* subList(int start, int end){
        auto len = end - start + ARRAY_GROW;
        auto nsize = end - start;
        auto n = new T[len];
        copyArray(arr, start, n, 0, nsize);
        return new ArrayList(n, nsize, len);
    }

    void growArray() {
        if (accSize == maxSize) {
            maxSize += ARRAY_GROW;
            T *tmp = new T[maxSize];
            copyArray(arr,0, tmp, 0, accSize);
            delete[] arr;
            arr = tmp;
        }
    }

    void copyArray(T *oldArr, int sourcePos, T *newArr, int destPos, int len) {
        for (int i = 0; i < len; i++) {
            newArr[destPos + i] = oldArr[sourcePos + i];
        }
    }



    void addAll(ArrayList<T> list) {
//        need some optimization with allocating memory!!!
        for (int i = 0; i < list.size(); i++) {
            add(list.get(i));
        }
    }

    int size() {
        return accSize;
    }

    int sizeInBytes(){
        return sizeof(T) * maxSize;
    }
};


#endif //MT_TEST_ARRAYLIST_H
