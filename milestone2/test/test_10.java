public class SwapNumbers {

    public static void main(String[] args) {

        float first = 1.20;
        float second = 2.45;

        System.out.println("--Before swap--");
        System.out.println(first);
        System.out.println(second);

        // Value of first is assigned to temporary
        float temporary = first+second;

        System.out.println("--After swap--");
        System.out.println(first);
        System.out.println(second);
    }
}