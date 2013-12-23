(defn check-login [username password]
     (let [encrypted-password (encrypt-string password)
           found-users (find-records {:login username
                                      :password encrypted-password})]
       (first found-users)))

