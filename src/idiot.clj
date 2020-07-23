(ns idiot
  (:require [clojure.java.io :as io])
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream InflaterInputStream)))

;; prints top-level usage message
(defn usage-m []
  (print "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n"))

;; print help usage message
(defn help-usage-m []
  (print "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n"))

;; print init usage message
(defn init-usage-m []
  (print "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message\n"))

;; print hash-object usage message
(defn hash-usage-m []
  (print "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file\n"))

;; print cat-file
(defn catf-usage-m []
  (print "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object\n"))

;; help command
(defn help
  ;; prints top level usage if no arguments given
  ([] (usage-m))
  ;; print usage message for different commands
  ;; tolerates and ignores extra arguments
  ([first & more] (cond
                    (= "-h" first) (help-usage-m)
                    (= "--help" first) (help-usage-m)
                    (= "help" first) (help-usage-m)
                    (= "init" first) (init-usage-m)
                    (= "hash-object" first) (hash-usage-m)
                    (= "cat-file" first) (catf-usage-m)
                    (> 0 (count more)) ()
                    :else (print "Error: invalid command\n"))))

;; sees if the .git directory exists in the current directory
(defn if-dir-exists []
  (.exists (io/file ".git")))

;; sees if file 'filename' is readable
(defn if-readable [filename]
  (.isFile (io/file filename)))

;; creates new .git directory in the current directory and objects directory inside .git
(defn init-git []
  (io/make-parents ".git/objects/child")
  (print "Initialized empty Idiot repository in .git directory\n"))

;; init command
(defn init
  ([] (if (if-dir-exists)
        (print "Error: .git directory already exists\n")
        (init-git)))
  ([arg1] (cond
            (= "-h" arg1) (init-usage-m)
            (= "--help" arg1) (init-usage-m)
            :else
            (print "Error: init accepts no arguments\n"))))

;; header+blob of file
(defn header-blob [filename]
  (let [length (count filename)]
    (str "blob " length "\000" filename)))

;; SHA1 address computation
(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))
(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))
(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))
(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

;; Zip file
(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn comp-address
  ([filename] (println (sha1-sum (header-blob (slurp filename)))))
  ([filename & more]
   (if (= "w" (first more))
     (let [contents (header-blob (slurp filename))
           address (sha1-sum (header-blob (slurp filename)))
           folder1 (subs address 0 2)
           folder2 (subs address 2)
           filedestination (str ".git/objects/" folder1 "/" folder2)]
       (println address)
       (io/make-parents filedestination)
       (io/copy (zip-str contents) (io/file filedestination)))
     ())))

;; called by hash-object if -w switch given
(defn hash-object-w
  ([] (print "Error: you must specify a file.\n"))
  ([filename] (if (not (if-readable filename))
                (println "Error: that file isn't readable\n")
                (comp-address filename "w"))))

;; hash-object command
(defn hash-object
  ([] (print "Error: you must specify a file.\n"))
  ([arg1 & more] (cond
                   (= "-h" arg1) (hash-usage-m)
                   (= "--help" arg1) (hash-usage-m)
                   (not (if-dir-exists)) (print "Error: could not find database. (Did you run `idiot init`?)\n")
                   (= "-w" arg1) (apply hash-object-w more)
                   (not (if-readable arg1)) (print "Error: that file isn't readable\n")
                   :else (comp-address arg1))))

;; see if file exists in objects
(defn if-object [address]
  (let [search (str ".git/objects/" address)]
    (if-readable search)))

;; unzip zipped file
(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

;; cat-file command
(defn cat-file
  ([] (print "Error: the -p switch is required\n"))
  ([arg1 & more] (cond
                   (= "-h" arg1) (catf-usage-m)
                   (= "--help" arg1) (catf-usage-m)
                   (not (if-dir-exists)) (print "Error: could not find database. (Did you run `idiot init`?)\n")
                   (not= "-p" arg1) (print "Error: the -p switch is required\n")
                   :else (cond
                           (= (count more) 0) (print "Error: you must specify an address\n")
                           (not (if-object (str (subs (first more) 0 2) "/" (subs (first more) 2)))) (print "Error: that address doesn't exist\n")
                           :else (let [address (str ".git/objects/" (str (subs (first more) 0 2) "/" (subs (first more) 2)))]
                                   (with-open [input (-> address io/file io/input-stream)]
                                     (let [input (str (unzip input))]
                                       (print (subs input (inc (.indexOf input "\000")))))))))))

(defn -main
  ([] (usage-m))
  ([s & more] (cond
                (= "-h" s) (usage-m)
                (= "--help" s) (usage-m)
                (= "help" s) (apply help more)
                (= "init" s) (apply init more)
                (= "hash-object" s) (apply hash-object more)
                (= "cat-file" s) (apply cat-file more)
                :else
                (print "Error: invalid command\n"))))