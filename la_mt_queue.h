#ifndef LA_MT_QUEUE_H
#define LA_MT_QUEUE_H

#include <iostream>
#include <mutex>
#include <optional>
#include <queue>

namespace la {

/**
 * @brief Thread-safe queue.
 *
 * @tparam T
 */
template<typename T>
    requires std::constructible_from<std::queue<T>> && std::move_constructible<T>
class mt_queue
{
public:
    using value_type = T;

    mt_queue()  = default;
    ~mt_queue() = default;

    mt_queue(const mt_queue<T>&)            = delete;
    mt_queue(mt_queue<T>&& other)           = delete;
    mt_queue& operator=(mt_queue<T>&&)      = delete;
    mt_queue& operator=(const mt_queue<T>&) = delete;

    /**
     * @brief Returns the number of elements in queue;
     *
     * @return std::size_t
     */
    [[nodiscard]] std::size_t size() const
    {
        std::scoped_lock lock(queue_mutex);
        return m_unsafe_queue.size();
    }

    /**
     * @brief If queue contains elements removes an element from the front of the queue
     * and returns optional containing removed element; If queue is empty returns empty optional;
     *
     * @return std::optional<T>
     */
    [[nodiscard]] auto pop() -> std::optional<T>
    {
        std::scoped_lock<std::mutex> lock(queue_mutex);
        if (m_unsafe_queue.empty())
        {
            return {};
        }
        std::optional<T> tmp = std::move(m_unsafe_queue.front());
        m_unsafe_queue.pop();
        return tmp;
    }

    /**
     * @brief Inserts the given element into the back of the queue;
     *
     * @param item
     */
    void push(const T& item)
        requires std::is_copy_constructible_v<T>
    {
        std::scoped_lock<std::mutex> lock(queue_mutex);
        m_unsafe_queue.push(item);
    }
    /**
     * @brief Inserts the given element into the back of the queue;
     *
     * @param item
     */
    void push(T&& item)
    {
        std::scoped_lock<std::mutex> lock(queue_mutex);
        m_unsafe_queue.push(std::forward<T>(item));
    }

private:
    std::queue<T>      m_unsafe_queue;
    mutable std::mutex queue_mutex;
};

}    // namespace la
#endif
