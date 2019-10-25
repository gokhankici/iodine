#ifndef IR_EXPORTER_HELPER_H
#define IR_EXPORTER_HELPER_H

#include <ostream>
#include <vector>

template <typename T>
std::ostream &operator<<(std::ostream &out,
                         const std::vector<T *, std::allocator<T *>> &elements)
{
    const unsigned element_count = elements.size();
    out << "[";
    if (element_count > 0)
    {
        out << *elements.at(0);
    }

    for (unsigned i = 1; i < element_count; i++)
    {
        out << ", " << *elements.at(i);
    }

    return out << "]";
}

template <typename T>
std::ostream &operator<<(std::ostream &out,
                         const std::vector<T, std::allocator<T>> &elements)
{
    const unsigned element_count = elements.size();
    out << "[";
    if (element_count > 0)
    {
        out << elements.at(0);
    }

    for (unsigned i = 1; i < element_count; i++)
    {
        out << ", " << elements.at(i);
    }

    return out << "]";
}

void backtrace();

#endif