using System;

namespace RedeMyLittlePoney.App.OpenGL
{
    /// <summary>
    /// The main class.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.ResultadoXOR = Algoritmo.algoritmoXor();
            Console.WriteLine(Application.ResultadoXOR);

            using (var game = new Game1())
                game.Run();
        }
    }
}
