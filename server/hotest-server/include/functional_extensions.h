#ifndef FUNCTIONAL_EXTENSIONS
#define FUNCTIONAL_EXTENSIONS

#include <functional>
#include <iterator>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <type_traits>

/**
 * A small template library provides tools
 * for more easy functional-programming style.
 */
namespace FunctionalExtensions {

template <typename T>
/**
 * @brief Maybe template class allows use values witch can
 *  has no value (like optional from C++17). For access data of Maybe use operator*,
 *  for check if it has value or not use cast Maybe for bool
 */
class Maybe {

public:
    /**
     * @brief Maybe constructor from value
     * @param val - value object will contain
     */
    Maybe(T val):_value(val), _hasValue(true) {}

    /**
     * @brief constructs Maybe object with no value (nothing)
     */
    Maybe () = default;

    /**
     * @brief if object has no value returns false, if value is present returns true
     */
    operator bool() const {
        return _hasValue;
    }

    /**
     * @brief Get value contained by Maybe object
     * @return value of the object of type T
     */
    T operator*(){
        return _value;
    }

private:
    T _value;
    bool _hasValue = false;

};

template <typename T>
/**
 * @brief just - constructs just Maybe object with value
 * @param t - value of new Maybe object
 * @return given t packed in Maybe
 */
Maybe<T> just(T t) {
    return Maybe<T>(t);
}

template <typename T>
/**
 * @brief nothing - constructs Maybe object with no value, i.e. nothing
 * @return Nothing packed in Maybe
 */
Maybe<T> nothing() {
    return Maybe<T>();
}

template <typename T, typename R, typename V = typename T::value_type>
/**
 * @brief map - apply function to each element of container
 * @param x - container which function will be applied for
 * @f f - function which will be applied for every element of container
 * @return new container of type T wich contains copied and transformed input data.
 */
R map(const T& x, V(*f)(V)) {
    R res(x.size());
    auto res_iter = begin(res);
    for (auto i(begin(x)); i < end(x); ++i) {
        *res_iter++ = f(*i);
    }
    return res;
}

template <typename T>
/**
 * @brief filter - make new container wich contains only elements passed condition f
 * @param x - input container
 * @param f - function of type (element) -> bool which is condition for filter
 * @return new filtered container created by copy and shrinked to size after filtering;
 */
T filter(const T& x, auto f) {
    T res(x.size());
    auto rend = std::copy_if(begin(x), end(x), begin(res), f);
    res.resize(std::distance(begin(res), rend));
    return res;
}

template <typename T>
/**
 * @brief fold - accumulate value from container with given function
 * @param x - container to be accumulated
 * @param init - initial value of accumulator
 * @param f - accumulating function of type elem -> accumulator -> accumulator. See std::accumulate.
 */
auto fold(const T& x, auto init, auto f){
    return std::accumulate(begin(x), end(x), init,  f);
}

template <typename T>
/**
 * @brief id - identity function, it just returns an argument
 * @param x - argument to be returned
 * @return returns given argument x
 */
T id(T x) {return x;}

}

#endif // FUNCTIONAL_EXTENSIONS

