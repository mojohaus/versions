package org.codehaus.mojo.versions.utils;

import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Miscellaneous utility class.
 */
public class MiscUtils {
    private MiscUtils() {
        // prevent instantiation
    }

    /**
     * Filters a given map leaving only elements fulfilling a predicate. Does not change the input map,
     * the filtered map is returned as output.
     *
     * @param map       input map to be filtered
     * @param predicate predicate for element comparison
     * @param <K>       key type
     * @param <V>       value type
     * @return map such that every element comforms with the predicate
     */
    public static <K, V> Map<K, V> filter(Map<K, V> map, Function<V, Boolean> predicate) {
        return map.entrySet().stream()
                .filter(e -> predicate.apply(e.getValue()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /**
     * Filters a given map leaving only elements fulfilling a predicate. Does not change the input map,
     * the filtered map is returned as output.
     *
     * @param map           input map to be filtered
     * @param predicate     predicate for element comparison
     * @param mergeFunction function to resolve collisions between values associated with the same key
     * @param <K>           key type
     * @param <V>           value type
     * @return map such that every element comforms with the predicate
     */
    public static <K, V> Map<K, V> filter(
            Map<K, V> map, Function<V, Boolean> predicate, BinaryOperator<V> mergeFunction) {
        return map.entrySet().stream()
                .filter(e -> predicate.apply(e.getValue()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, mergeFunction));
    }
}
