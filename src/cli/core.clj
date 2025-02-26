(ns cli.core
  (:gen-class)
  (:import [javax.imageio ImageIO]
           [java.io File]
           [java.awt.image BufferedImage]
           [org.bytedeco.javacv FFmpegFrameGrabber Java2DFrameConverter])
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

(defn save-frame
  [frame index]
  (try
    (let [output-dir (File. "debug-frames")
          output-file (File. output-dir (str "frame-" index ".png"))]
      (when-not (.exists output-dir)
        (.mkdir output-dir))
      (ImageIO/write frame "png" output-file))
    (catch Exception e
      (println "Error saving frame " index ": " e))))

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
     :pixels (for [y (range 0 height (* 2.5 downsample-factor))
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
  ;; (print "\u001B[2J")
  (let [buffer (StringBuilder.)]
    (doseq [line ascii]
      (.append buffer (str (apply str line) (:reset ansi-colour)))
      (.append buffer "\n"))
    (print (.toString buffer))
    (flush)))

(defn extract-gif-frames
  [filename]
  (let [file (File. filename)
        input (ImageIO/createImageInputStream file)
        readers (ImageIO/getImageReaders input)
        reader (first (iterator-seq readers))]
    (.setInput reader input)
    (let [num-frames (.getNumImages reader true)]
      (println "Number of frames in the GIF: " num-frames)
      (loop [frames [] index 0]
        (if (< index num-frames)
            ;; (if-let [frame (.read reader index)]
          (recur (conj frames (.read reader index)) (inc index))
          frames)))))

(defn gif-to-ascii
  [filename downsample-factor coloured? extended?]
  (let [frames (extract-gif-frames filename)]
    (if (empty? frames)
      (println "No frames extracted from the GIF.")
      (do
        (println (str "Extracted " (count frames) " frames from the GIF."))
        (doall (map #(-> %
                  (get-pixel-data downsample-factor)
                  (to-ascii coloured? extended?))
             frames))))))

(defn clone-buffered-image [image]
  (let [cm (.getColorModel image)
        isAlphaPremultiplied (.isAlphaPremultiplied cm)
        raster (.copyData image nil)]
    (BufferedImage. cm raster isAlphaPremultiplied nil)))

(defn extract-mp4-frames
  [filename]
  (let [grabber (FFmpegFrameGrabber. filename)
        converter (Java2DFrameConverter.)]
    (.start grabber)
    (let [total-frames (.getLengthInFrames grabber)
          frame-rate (.getFrameRate grabber)]
      (println "Total frames in the video: " total-frames)
      (println "Frame rate: " frame-rate)
      (let [frames (loop [frames [] frame-count 0]
                     (if (>= frame-count total-frames)
                       frames
                       (do
                         (.setVideoFrameNumber grabber frame-count)
                         (let [frame (.grab grabber)]
                           (if (nil? frame)
                             (recur frames (inc frame-count))
                             (let [buffered-image (.convert converter frame)]
                               (if (nil? buffered-image)
                                 (do
                                   (println "Warning: Skipping null buffered image")
                                   (recur frames (inc frame-count)))
                                 (do
                                  ;;  (println "Extracted Frame: " frame-count)
                                   (let [cloned-image (clone-buffered-image buffered-image)]
                                    ;;  (save-frame cloned-image frame-count)
                                     (recur (conj frames cloned-image) (inc frame-count)))))))))))]
        (.stop grabber)
        frames))))

(defn mp4-to-ascii
  [filename downsample-factor coloured? extended?]
  (let [frames (extract-mp4-frames filename)]
    (if (empty? frames)
      (println "No frames extracted from the MP4.")
      (do
        (println (str "Extracted " (count frames) " frames from the MP4"))
        (doall (pmap (fn [frame]
                       (-> frame
                           (get-pixel-data downsample-factor)
                           (to-ascii coloured? extended?)))
                     frames))))))

(defn play-gif-ascii
  [ascii-frames]
  (println "\u001B[2J")
  (let [frame-delay (/ 1000 30)
        start-time (System/currentTimeMillis)]
  (doseq [frame ascii-frames]
  (let [frame-start (System/currentTimeMillis)]
    (println "\u001B[H")
    (print-ascii frame)
    (let [procesesing-time (- (System/currentTimeMillis) frame-start)
          sleep-time (max 0 (- frame-delay processing-time))]
      (when (pos? sleep-time)
        (Thread/sleep sleep-time)))))
    ;; (Thread/sleep 10))
  (println "Finished")))
  ;; (println "\u001B[2J" "\u001B[H")))

(defn enter-to-continue
  []
  (println "Press enter to continue...")
  (loop []
    (when (not= (read-line) "")
      (recur))))

(defn ascii-mp4
  []
  (try
    (let [filename (do (println "Enter MP4 filename:") (read-line))
          downsample-factor (do (println "Enter a downsample factor (e.g. 4 - meaning 4x smaller):") (Integer/parseInt (read-line)))
          coloured? (do (println "Do you want the ASCII gif to be coloured? (y/n):") (= "y" (read-line)))
          extended? (do (println "Do you want to use a larger range of ascii characters? (y/n):") (= "y" (read-line)))]
      (-> filename
          (mp4-to-ascii downsample-factor coloured? extended?)
          play-gif-ascii))
    (catch Exception e
      (println "Error: Invalid filename or unable to read the gif: " e)))
  (enter-to-continue)
  {:menu :main})

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

(defn detect-edges
  [image]
  (let [width (.getWidth image)
        height (.getHeight image)
        result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)

        sobel-x (flatten [[-1 0 1] [-2 0 2] [-1 0 1]])
        sobel-y (flatten [[1 2 1] [0 0 0] [-1 -2 -1]])

        get-grayscale (fn [rgb]
                        (let [r (bit-and (bit-shift-right rgb 16) 0xFF)
                              g (bit-and (bit-shift-right rgb 8) 0xFF)
                              b (bit-and rgb 0xFF)]
                          (int (/ (+ r g b) 3))))
        get-rgb (fn [x y]
                  (if (and (>= x 0) (< x width) (>= y 0) (< y height))
                    (.getRGB image x y)
                    0))

        get-neighborhood (fn [x y]
                           (for [dy (range -1 2)
                                 dx (range -1 2)]
                             (get-grayscale (get-rgb (+ x dx) (+ y dy)))))
        apply-kernel (fn [neighbourhood kernel]
                       (reduce + (map * kernel neighbourhood)))]
                       (doseq [y (range height)
                               x (range width)]
                         (let [neighbourhood (get-neighborhood x y)
                               gx (apply-kernel neighbourhood sobel-x)
                               gy (apply-kernel neighbourhood sobel-y)
                               magnitude (int (Math/sqrt (+ (* gx gx) (* gy gy))))
                               normalized (min 255 magnitude)
                               edge-pixel (+ (* normalized 0x010101))]
                           (.setRGB result x y edge-pixel)))
                       result))

(defn enhance-with-edges
  "More concise implementation of edge enhancement"
  [image edge-weight]
  (let [width (.getWidth image)
        height (.getHeight image)
        edges (detect-edges image)
        result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    
    (doseq [y (range height)
            x (range width)]
      (let [original-rgb (.getRGB image x y)
            edge-value (bit-and (.getRGB edges x y) 0xFF)
            
            ;; Extract RGB components
            orig-r (bit-and (bit-shift-right original-rgb 16) 0xFF)
            orig-g (bit-and (bit-shift-right original-rgb 8) 0xFF)
            orig-b (bit-and original-rgb 0xFF)
            
            ;; Calculate enhanced values
            enhance-channel (fn [channel]
                              (max 0 (min 255 (- channel (int (* edge-value edge-weight))))))
            
            enhanced-rgb (bit-or (bit-shift-left (enhance-channel orig-r) 16)
                                (bit-shift-left (enhance-channel orig-g) 8)
                                (enhance-channel orig-b))]
        
        (.setRGB result x y enhanced-rgb)))
    
    result))

(defn ascii-image
  []
  (try
    (let [filename (do (println "Enter filename:") (read-line))
          downsample-factor (do (println "Enter a downsample factor (e.g. 4 - meaning 4x smaller):") (Integer/parseInt (read-line)))
          coloured? (do (println "Do you want the ASCII image to be coloured? (y/n):")
                        (= "y" (read-line)))
          extended? (do (println "Do you want to use a larger range of ascii characters? (y/n):")
                        (= "y" (read-line)))
          edge-enhance? (do (println "Do you want to enhance edges? (y/n):")
                            (= "y" (read-line)))
          edge-weight (if edge-enhance?
                        (do (println "Enter edge enhancement strength (0.1 - 1.0):")
                            (Double/parseDouble (read-line)))
                        0.0) 
          img (read-image filename)
          processed-img (if edge-enhance?
                          (enhance-with-edges img edge-weight)
                          img)
          ] 
      (println "Processing image...")
      (-> processed-img
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
    (let [query (do (println "Enter query:") (read-line))
          result (jdbc/execute! ds [query] {:builder-fn rs/as-unqualified-lower-maps})]
      (if (empty? result)
        (println "No results returned from query")
        (let [column-names (keys (first result))
              column-count (count column-names)
              column-widths (reduce (fn [widths row]
                                      (reduce-kv (fn [w k v]
                                                   (update w k max (count (str v))))
                                                 widths
                                                 row))
                                    (zipmap column-names (map #(count (str %)) column-names))
                                    result)]
          (println "Number of columns:" column-count)
          (println "Column names:" (pr-str column-names))
          (println "Column widths:" (pr-str column-widths) "\n\n")

          (doseq [col column-names]
            (print (format (str "%-" (get column-widths col) "s ") (name col))))
          (println)

          (doseq [col column-names]
            (print (str (apply str (repeat (get column-widths col) "-")) " ")))
          (println)

          (doseq [row result]
            (doseq [col column-names]
              (print (format (str "%-" (get column-widths col) "s ") (str (get row col)))))
            (println)))))

      ;; (println "Query result: " (:isu_email (first result))))
      ;; (map :isu_email result))
      ;; (doseq [row result]
      ;;   (println "Email: " (:isu_email row))))
      ;; (println result)
      ;; (println "map: " (map :isu_email result))
    (enter-to-continue) 
    {:menu :mysql}))

(def mysql-menu-options
  {:title "MySQL"
   :options [;;  {:title "Query" :function mysql-query} 
             {:title "Connect" :function mysql-connect}
             {:title "Back" :function (fn [] {:menu :main})}
             {:title "Exit" :function (fn [] (System/exit 0))}]})

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
             {:title "Convert MP4 to ASCII" :function ascii-mp4}
             {:title "Print available ANSI Colours" :function print-256-colours}
             {:title "MySQL" :function (fn [] {:menu :mysql})}
             {:title "Exit" :function (fn [] (System/exit 0))}]})

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