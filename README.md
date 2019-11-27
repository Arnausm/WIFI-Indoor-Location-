WIFI-Indoor-Location
Project Goal:
Understanding customer behavior is essential for any business aiming to provide a more personal and compelling shopping experience, optimize store layout, and improve store operations. Achieving these goals ultimately leads to improved user experience, conversion rates, and increased revenue. Today’s mobile-based location technologies provide information about the user’s location that can be leveraged for advanced analytics and visualizations. This means retailers and enterprises can gain insight into customer behavior patterns and understand, for example, how much time customers spend in different areas of the store, what are the routes they take, how well they are serviced, and more.

Automatic user localization consists of estimating the position of the user (latitude, longitude and altitude) by using an electronic device, usually a mobile phone. Mobile devices include many different technologies that enable the mobile device user’s location to be identified. Global Positioning System (GPS) and cellular triangulation are available in all modern smartphones; however, these technologies are not effective for providing location data when users are in indoor localization due to the loss of GPS signal in indoor environments.

The indoor location market is a convergence of a number of technologies used to determine the location of a person or an asset indoors, leveraging technologies like WiFi, Bluetooth, Radiofrequency identification (RFID), and location sensor technologies such as accelerometers and gyroscopes as alternative or complementary technologies to GPS.
Dataset Details:

The database covers three buildings of Universitat Jaume I with 4 or more floors and almost 110.000m2. It can be used for classification, e.g. actual building and floor identification, or regression, e.g. actual longitude and latitude estimation. It was created in 2013 by means of more than 20 different users and 25 Android devices. The database consists of 19937 training/reference records (trainingData.csv file) and 1111 validation/test records (validationData.csv file).

The 529 attributes contain the WiFi fingerprint, the coordinates where it was taken, and other useful information.
Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging -104dBm (extremely poor signal) to 0dbM. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.
Then the coordinates (latitude, longitude, floor) and Building ID are provided as the attributes to be predicted.
Additional information has been provided.
The particular space (offices, labs, etc.) and the relative position (inside/outside the space) where the capture was taken have been recorded. Outside means that the capture was taken in front of the door of the space.
Information about who (user), how (android device & version) and when (timestamp) WiFi capture was taken is also recorded.
