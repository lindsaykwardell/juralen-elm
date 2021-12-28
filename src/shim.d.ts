declare module "*.elm" {
    export const Elm: unknown
}

declare module "*.mp3" {
    const value: string
    export default value
}
