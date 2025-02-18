(ns cli.core 
  (:gen-class)
  (:import [javax.imageio ImageIO] 
           [java.io File] 
           [java.awt.image BufferedImage]
           )
  (:require [next.jdbc :as jdbc]))

(def logo "
   #############   #######        #############        #######   #######  #######  ###############   ~##############           #############   #######       #######   
   #############    #####        !#############-        +####     #####    #####    ##############     #############           #############    #####         #####    
   #####   &####    #####        !####     ####-        ~####     #####    #####    #####    +####     ####    #####           #####    ####    #####         #####    
   #####   &####    #####        !####     ####-        ~####     #####    #####    #####    +####     ####    ##              #####    ####    #####         #####    
   #####    ~       #####        !####     ####-        ~####     #####    #####    #####    +####     ####                    #####    !       #####         #####    
   #####            #####        !####     ####-        ~####     #####    #####    #####    #####     ####   ##               #####            #####         #####    
   #####            #####        !####     ####-        ~####     #####    #####    ##### #######&     #########               #####            #####         #####    
   #####            #####        !####     ####-        ~####     #####    #####    ###########        #########               #####            #####         #####    
   #####      ###   #####        !####     ####-     +  ~####     #####    #####    ############       ######                  #####      ###   #####         #####    
   #####    #####   #####   #### !####     ####-  ####  ~####     #####    #####    #####  #####!      ####     ####           #####    #####   #####   ####  #####    
   #####    #####   #####  ##### !####     ####-  ####  ~####     #####    #####    #####   #####      ####    #####           #####    #####   #####  #####  #####    
   ##############   ############ !#############-  ###########     ##############    #####    #####     #############           ##############   ############  #####    
   #############   #############  #############   ###########     #############    #######   ######~  ##############           #############   ############# #######   
                                                                                                                                                                    
                                                                                                                                                                    
           ")

(def ansi-colour
  {:reset "\u001B[0m" 
   :white "\u001B[37m"
   :black "\u001B[30m"
   :red "\u001B[31m"
   :green "\u001B[32m"
   :yellow "\u001B[33m"
   :blue "\u001B[34m"
   :purple "\u001B[35m"
   :cyan "\u001B[36m"
   :bg-white "\u001B[47m"
   :bg-black "\u001B[40m"
   :bg-red "\u001B[41m"
   :bg-green "\u001B[42m"
   :bg-yellow "\u001B[43m"
   :bg-blue "\u001B[44m"
   :bg-purple "\u001B[45m"
   :bg-cyan "\u001B[46m"})

(def lookup-table " .,-~+=!&#@")
;; (def lookup-table " .:-=+x*!?#%@")
;; (def lookup-table " .:-=+*#%@")
;; (def lookup-table (clojure.string/reverse "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "))

(defn read-image
  [filename]
  (let [file (File. filename)
        image (ImageIO/read file)]
    image))

;; Getting the pixel data from the image
(defn get-pixel-data 
  [image downsample-factor] 
  (let [width (.getWidth image) 
        height (.getHeight image)] 
    {:width (int (Math/ceil (/ width downsample-factor)))
     :height (int (Math/ceil (/ height downsample-factor)))
     :pixels (for [y (range 0 height (* 2 downsample-factor)) 
                   x (range 0 width downsample-factor)] 
               (.getRGB image x y))}))

;; Getting the brightness values from the pixel data
(defn brightness
  [rgb]
  (let [red (bit-and (bit-shift-right rgb 16) 0xFF)
        green (bit-and (bit-shift-right rgb 8) 0xFF)
        blue (bit-and rgb 0xFF)
        brightness (Math/sqrt (+ (* 0.299 (* red red)) (* 0.586 (* green green)) (* 0.114 (* blue blue))))
        r (int (/ red 51)) 
        g (int (/ green 51)) 
        b (int (/ blue 51)) 
        colour-code (+ 16 (* 36 r) (* 6 g) b)]
      {:brightness brightness :colour-code colour-code}))

;; Converting the brightness values to ascii characters
(defn brightness-to-ascii
  [{:keys [brightness colour-code]}]
  (let [index (int (* (/ brightness 255) (dec (count lookup-table))))
        char (get lookup-table index)
        colour (str "\u001B[38;5;" colour-code "m")]
    (str colour char (:reset ansi-colour))))

;; Convert the pixel data to ascii characters
(defn to-ascii
  [{:keys [width pixels]}]
  (let [brightness-values (map brightness pixels) 
        ascii-chars (map brightness-to-ascii brightness-values)]
    (partition width ascii-chars)))

;; Print the ascii characters to the console
(defn print-ascii
  [ascii]
  (doseq [line ascii]
    (println (str (apply str line) (:reset ansi-colour)))))

(defn enter-to-continue
  []
  (println "Press enter to continue...")
  (loop []
    (when (not= (read-line) "")
      (recur))))

;; First version
;; (defn ascii-image
;;   [filename downsample-factor]
;;   (-> filename
;;       read-image
;;       (get-pixel-data downsample-factor)
;;       to-ascii
;;       print-ascii))

;; Second version to handle input
(defn ascii-image
  []
  (println "Enter filename followed by a downsample factor (e.g. 4 - meaning 4x smaller):")
  (try 
    (let [filename (read-line) 
          downsample-factor (Integer/parseInt (read-line))] 
      (-> filename 
          read-image 
          (get-pixel-data downsample-factor) 
          to-ascii 
          print-ascii)) 
    (catch Exception e 
      (println "Error: Invalid filename or unable to read the image."))) 
  (enter-to-continue)
  {:menu :main})

(defn print-256-colours
  []
  (doseq [colour-code (range 256)]
    (println (str "\u001B[38;5;" colour-code "m" "Colour" colour-code (:reset ansi-colour)) (str "\u001B[48;5;" colour-code "m" "Colour" colour-code (:reset ansi-colour)))) 
  (enter-to-continue)
  {:menu :main})

(defn mysql-query
  []
  (println "")
  (enter-to-continue)
  {:menu :mysql})

(defn mysql-connect
  []
  (println "Please enter the MySQL Connection details:")
  (let [dbtype (read-line)
        dbname (read-line)
        dbuser (read-line)
        dbpassword (read-line)
        ds (jdbc/get-datasource {:dbtype dbtype :dbname dbname :dbuser dbuser :dbpassword dbpassword})]
    (println "Connected to db")
    (jdbc/execute! ds ["SELECT * from table"])
    (enter-to-continue)
    {:menu :mysql}))

(def mysql-menu-options
  {:title "MySQL"
   :options [
            ;;  {:title "Query" :function mysql-query} 
            {:title "Connect" :function mysql-connect}
             {:title "Back" :function (fn [] {:menu :main})} 
             {:title "Exit" :function (fn [] (System/exit 0))}
             ]})

;; So since this is changing to a proper like command line tool, we need to adjust and have a interface main menu etc.
;; Means we need a function that prints out a menu
;; function to get the user input
;; function to handle user input to commands
;; I liked the Haskell idea of data type for menu options, so we can have a list of menu options and within this menu option it has a title + function to call
;; Then we can just call the function and return
;; We need a state to keep track of the current menu and the state of the program
(def main-menu-options 
  {:title "Main Menu" 
   :options [{:title "Convert Image to ASCII" :function ascii-image} 
             {:title "Print available ANSI Colours" :function print-256-colours} 
             {:title "MySQL" :function (fn [] {:menu :mysql})} 
             {:title "Exit" :function (fn [] (System/exit 0))}
             ]})

(defn print-menu
  [menu]
  (println "\u001B[2J" "\u001B[H")
  (println logo)
  (println (:title menu))
  (doseq [[index menu-option] (map-indexed vector (:options menu))]
    (println (str index ". " (:title menu-option)))))

(defn get-user-input
  []
  (read-line))

(defn handle-user-input
  [user-input menu]
  (try 
    (let [index (Integer/parseInt user-input) 
          menu-option (get menu index)] 
      (if menu-option 
        (let [function (:function menu-option)] 
          (function)) 
        {:menu :main}))
    (catch Exception e
      (println "Error: Invalid input.")
      (enter-to-continue)
      {:menu :main})))

;; (defn -main
;;   [& args
;;   (println logo)
;;   (println "Enter filename:") 
;;   ;; (println "Available ANSI Colours:")
;;   ;; (print-256-colours)
;;   ;; (println :reset ansi-colour)
;;   (let [filename (read-line)] 
;;     (println "Enter downsample factor:") 
;;     (let [downsample-factor (Integer/parseInt (read-line))] 
;;       (ascii-image filename downsample-factor))))

;; Wanting to implement the same mysql query option instead of having to always use gems to communicate


(defn -main
  [& args]
  (loop [state {:menu :main}]
    (case (:menu state)
      :main
      (do
        (print-menu main-menu-options)
        (let [user-input (get-user-input)]
          (recur (handle-user-input user-input (:options main-menu-options)))))
      :mysql
      (do
        (print-menu mysql-menu-options)
        (let [user-input (get-user-input)]
          (recur (handle-user-input user-input (:options mysql-menu-options))))))))