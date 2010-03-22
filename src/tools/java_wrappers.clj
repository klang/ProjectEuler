(ns tools.java-wrappers)

(defn free-mem [] (.freeMemory (Runtime/getRuntime)))
(defn total-mem [] (.totalMemory (Runtime/getRuntime)))
(defn max-mem [] (.maxMemory (Runtime/getRuntime)))
