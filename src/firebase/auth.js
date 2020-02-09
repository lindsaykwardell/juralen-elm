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

    // ports.register.subscribe(({ email, password }) => {
    //     firebase
    //         .auth()
    //         .createUserWithEmailAndPassword(
    //             this.state.emailAddress,
    //             this.state.password
    //         )
    //         .then(() => {
    //             const user = firebase.auth().currentUser

    //             user.updateProfile({
    //                 displayName: "Lord Knave"
    //             })
    //         })
    //         .catch(error => {
    //             console.log(error)
    //         })
    // })
}
