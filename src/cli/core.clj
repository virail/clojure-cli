(ns cli.core 
  (:gen-class)
  (:import [javax.imageio ImageIO] 
           [java.io File] 
           [java.awt.image BufferedImage]))

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

(defn ascii-image
  [filename downsample-factor]
  (-> filename
      read-image
      (get-pixel-data downsample-factor)
      to-ascii
      print-ascii))

(defn print-256-colours
  []
  (doseq [colour-code (range 256)]
    (println (str "\u001B[38;5;" colour-code "m" "Colour" colour-code (:reset ansi-colour)) (str "\u001B[48;5;" colour-code "m" "Colour" colour-code (:reset ansi-colour)))))

;; So since this is changing to a proper like command line tool, we need to adjust and have a interface main menu etc.

(defn -main
  [& args]
  (println logo)
  (println "Enter filename:") 
  ;; (println "Available ANSI Colours:")
  ;; (print-256-colours)
  ;; (println :reset ansi-colour)
  (let [filename (read-line)] 
    (println "Enter downsample factor:") 
    (let [downsample-factor (Integer/parseInt (read-line))] 
      (ascii-image filename downsample-factor))))
