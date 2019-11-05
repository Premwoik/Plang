//
// Created by prw on 10.10.2019.
//

#ifndef MT_TEST_MAP_H
#define MT_TEST_MAP_H

#include "ArrayList.h"
#include "Maybe.h"




template<typename K, typename V>
class Map {
private:
    struct Pair {
        K key;
        V value;

        Pair(K key, V val) : key(key), value(val) {}
    };

    ArrayList<Pair *> storage;

    int posOfKey(K key){
        for(auto i = 0; i < storage.size(); i++){
            auto v = storage.get(i);
            if (v->key == key){
                return i;
            }
        }
        return -1;
    }

public:

    Map() {}

    void put(K key, V val) {
        auto guard = false;
        for (auto i = 0; i < storage.size(); i++) {
            auto s = storage.get(i);
            if (s->key == key) {
                s->value = val;
                guard = true;
            }
        }
        if (!guard) {
            auto p = new Pair(key, val);
            storage.add(p);
        }
    }

    Maybe<V> get(K key) {
        for (auto i = 0; i < storage.size(); i++) {
            auto s = storage.get(i);
            if (s->key == key) {
                return Maybe<V>(s->value);
            }
        }
        return Maybe<V>();
    }



    Maybe <V> remove (K key){
        auto pos = posOfKey(key);
        if(pos > -1){
            auto p = get(key).get();
            return Maybe<V>(p);
        }
        return Maybe<V>();
    }
};


#endif //MT_TEST_MAP_H
