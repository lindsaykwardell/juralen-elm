import firebase from "./init"

export default ports => {
    firebase.auth().onAuthStateChanged(user => {
        if (user) {
            //   this.props.updateName(firebase.auth().currentUser.displayName);
            ports.authStatus.send(true)
        } else {
            ports.authStatus.send(false)
        }
    })

    ports.logout.subscribe(() => {
        firebase.auth().signOut()
    })

    ports.login.subscribe(({ email, password }) => {
        firebase
            .auth()
            .signInWithEmailAndPassword(email, password)
            .catch(error => {
                console.log(error)
            })
    })
}
