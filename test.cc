#include <iostream>
#include <vector>

auto f(std::vector<int>& v) -> int* {
    for (int& i : v) {
        return &i;
    }

    return nullptr;
}

int main(void) {
    std::vector<int> a{1, 2,3, 5};

    std::cout << f(a) << std::endl;
    return 0;
}
