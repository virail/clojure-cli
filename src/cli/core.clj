(ns cli.core 
  (:gen-class)
  (:import [javax.imageio ImageIO]
           [java.io File]
           [java.awt.image BufferedImage])
  (:require [next.jdbc :as jdbc])
  (:require [next.jdbc.result-set :as rs]))

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
(def extended-lookup-table (clojure.string/reverse "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "))
(def extended-lookup-table " `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@")

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
  [{:keys [brightness colour-code]} coloured? extended?]
  (let [index (int (* (/ brightness 255) (dec (count (if extended? extended-lookup-table lookup-table)))))
        char (get (if extended? extended-lookup-table lookup-table) index)
        colour (if coloured?
                 (str "\u001B[38;5;" colour-code "m")
                 "")]
    (str colour char (:reset ansi-colour))))

;; Convert the pixel data to ascii characters
(defn to-ascii
  [{:keys [width pixels]} coloured? extended?]
  (let [brightness-values (pmap brightness pixels) 
        ascii-chars (pmap #(brightness-to-ascii % coloured? extended?) brightness-values)]
    (partition width ascii-chars)))

;; Print the ascii characters to the console
(defn print-ascii
  [ascii]
  (doseq [line ascii]
    (println (str (apply str line) (:reset ansi-colour)))))

(defn extract-gif-frames
  [filename] 
  (let [file (File. filename)
        reader (ImageIO/getImageReadersByFormatName "gif")
        reader (first (iterator-seq reader))]
  (.setInput reader (ImageIO/createImageInputStream file) false)
  (let [num-frames (.getNumImages reader true)]
    (loop [frames [] index 0]
      (if (< index num-frames)
        (if-let [frame (.read reader index)]
            (recur (conj frames frame) (inc index))
            frames))))))

(defn gif-to-ascii
  [filename downsample-factor coloured? extended?]
  (let [frames (extract-gif-frames filename)]
    (map #(-> %
              (get-pixel-data downsample-factor)
              (to-ascii coloured? extended?)) 
        frames)))

(defn play-gif-ascii
  [ascii-frames]
  (doseq [frame ascii-frames]
    (print-ascii frame)
    (Thread/sleep 100))
  (println "Finished"))
  ;; (println "\u001B[2J" "\u001B[H")))

(defn enter-to-continue
  []
  (println "Press enter to continue...")
  (loop []
    (when (not= (read-line) "")
      (recur))))

(defn ascii-gif
  []
  (try
    (let [filename (do (println "Enter GIF filename:") (read-line))
          downsample-factor (do (println "Enter a downsample factor (e.g. 4 - meaning 4x smaller):") (Integer/parseInt (read-line)))
          coloured? (do (println "Do you want the ASCII gif to be coloured? (y/n):") (= "y" (read-line)))
          extended? (do (println "Do you want to use a larger range of ascii characters? (y/n):") (= "y" (read-line)))]
      (-> filename
          (gif-to-ascii downsample-factor coloured? extended?)
          play-gif-ascii))
    (catch Exception e
      (println "Error: Invalid filename or unable to read the gif: " e)))
  (enter-to-continue)
  {:menu :main})

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
  (try 
    (let [filename (do (println "Enter filename:") (read-line)) 
          downsample-factor (do (println "Enter a downsample factor (e.g. 4 - meaning 4x smaller):") (Integer/parseInt (read-line)))
          coloured? (do (println "Do you want the ASCII image to be coloured? (y/n):")
                        (= "y" (read-line)))
                        extended? (do (println "Do you want to use a larger range of ascii characters? (y/n):")
                        (= "y" (read-line)))] 
      (-> filename 
          read-image 
          (get-pixel-data downsample-factor) 
          (to-ascii coloured? extended?)
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
  (let [dbtype (do (println "DB Type") (read-line))
        dbname (do (println "DB Name") (read-line))
        username (do (println "Username") (read-line))
        password (do (println "Password") (read-line))
        ds (jdbc/get-datasource {:dbtype dbtype :dbname dbname :user username :password password})]
    (if ds
      (println "Connected to db")
      (println "Failed to connect to db"))
    (let [result (jdbc/execute! ds ["SELECT * from intranet_shop_users"] {:builder-fn rs/as-unqualified-lower-maps})]
      ;; (println "Query result: " (:isu_email (first result))))
      ;; (map :isu_email result))
      (doseq [row result]
      (println "Email: " (:isu_email row))))
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
             {:title "Convert GIF to ASCII" :function ascii-gif}
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
      (println "Error: Invalid input." e)
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