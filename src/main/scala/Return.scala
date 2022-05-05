package return_class

class Return(var value: Any) extends RuntimeException {
    def main(value: Any) = {
        // super.main(null, null, false, false)
        this.value = value
    }
}