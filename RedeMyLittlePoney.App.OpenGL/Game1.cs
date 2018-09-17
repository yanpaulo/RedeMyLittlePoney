using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

using MVector = MathNet.Numerics.LinearAlgebra.Vector<double>;

namespace RedeMyLittlePoney.App.OpenGL
{
    /// <summary>
    /// This is the main type for your game.
    /// </summary>
    public class Game1 : Game
    {
        GraphicsDeviceManager graphics;
        SpriteBatch spriteBatch;

        internal List<PointColor> CoresFundo { get; private set; }
        internal IEnumerable<PointColor> PontosXOR { get; private set; }
        internal IEnumerable<PointColor> PontosRegressao { get; private set; }
        internal IEnumerable<PointColor> PontosFuncao { get; private set; }

        private Texture2D white, gray;

        public Game1()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        /// <summary>
        /// Allows the game to perform any initialization it needs to before starting to run.
        /// This is where it can query for any required services and load any non-graphic
        /// related content.  Calling base.Initialize will enumerate through any components
        /// and initialize them as well.
        /// </summary>
        protected override void Initialize()
        {
            // TODO: Add your initialization logic here
            //Algoritmo

            base.Initialize();
        }

        /// <summary>
        /// LoadContent will be called once per game and is the place to load
        /// all of your content.
        /// </summary>
        protected override void LoadContent()
        {
            // Create a new SpriteBatch, which can be used to draw textures.
            spriteBatch = new SpriteBatch(GraphicsDevice);
            LoadClassificacao();
            LoadRegressao();

            white = new Texture2D(GraphicsDevice, 1, 1);
            white.SetData(new[] { Color.White });

            gray = new Texture2D(GraphicsDevice, 1, 1);
            gray.SetData(new[] { new Color(Color.DarkGray, 0.7f) });
            // TODO: use this.Content to load your game content here
        }

        private void LoadRegressao()
        {
            var w = Application.ResultadoRegressao.Melhor.W;

            PontosRegressao = Enumerable
                .Range(0, 100)
                .Select(n => new PointColor
                {
                    Point = new Point(n, (int)(Algoritmo.resultadoLinear(w, MVector.Build.Dense(new[] { n / 100.0d }))[0] * 5)),
                    Color = Color.Red
                });

            PontosFuncao = Enumerable
                .Range(0, 100)
                .Select(n => new PointColor
                {
                    Point = new Point(n, (int)(Algoritmo.funcaoRegessao(n / 10.0d) * 10.0d)),
                    Color = Color.Blue
                });
        }

        private void LoadClassificacao()
        {
            var modelo = Application.ResultadoXOR.Melhor.W;
            var classes = new List<MVector>(Algoritmo.classesXorSeq);
            var dados = new List<Algoritmo.Par>(Algoritmo.dadosXorSeq);

            var cores = new Dictionary<MVector, Color>
            {
                {classes[0], Color.Red },
                {classes[1], Color.Blue },
            };

            //Algoritmo.resultado(
            var pixelsFundo = Enumerable
                .Range(0, 100)
                .Select(x =>
                    Enumerable
                    .Range(0, 100)
                    .Select(y => new Point(x, y)))
                .SelectMany(e => e)
                .ToList();

            CoresFundo = pixelsFundo
                .Select(p => new PointColor
                {
                    Point = p,
                    Color = cores.TryGetValue(Algoritmo.resultadoSigmoide(modelo, MVector.Build.Dense(new[] { p.X / 100.0d, p.Y / 100.0d })), out var v) ? v : Color.White
                })
                .ToList();

            PontosXOR = dados
                .Select(par => new PointColor
                {
                    Point = new Point((int)(par.X[0] * 100), (int)(par.X[1] * 100)),
                    Color = cores[par.Y]
                });
        }

        /// <summary>
        /// UnloadContent will be called once per game and is the place to unload
        /// game-specific content.
        /// </summary>
        protected override void UnloadContent()
        {
            // TODO: Unload any non ContentManager content here
        }

        /// <summary>
        /// Allows the game to run logic such as updating the world,
        /// checking for collisions, gathering input, and playing audio.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Update(GameTime gameTime)
        {
            if (GamePad.GetState(PlayerIndex.One).Buttons.Back == ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
                Exit();

            // TODO: Add your update logic here

            base.Update(gameTime);
        }

        /// <summary>
        /// This is called when the game should draw itself.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.White);
            // TODO: Add your drawing code here
            Matrix m;

            m = Matrix.CreateScale(2.0f) * Matrix.CreateTranslation(new Vector3(GraphicsDevice.Viewport.Bounds.Width / 2 - 100, 50.0f, 0.0f));

            spriteBatch.Begin(transformMatrix: m);
            foreach (var cor in CoresFundo)
            {
                spriteBatch.Draw(gray, cor.Point.ToVector2(), cor.Color);
            }

            foreach (var cor in PontosXOR)
            {
                var rect = new Rectangle(cor.Point.X - 2, cor.Point.Y - 2, 4, 4);
                spriteBatch.Draw(white, cor.Point.ToVector2(), cor.Color);
            }
            spriteBatch.End();

            m = Matrix.CreateScale(2.0f) * Matrix.CreateTranslation(new Vector3(GraphicsDevice.Viewport.Bounds.Width / 2 - 100, 300.0f, 0.0f));

            spriteBatch.Begin(transformMatrix: m);

            foreach (var cor in PontosFuncao)
            {
                var rect = new Rectangle(cor.Point.X - 2, cor.Point.Y - 2, 4, 4);
                spriteBatch.Draw(white, cor.Point.ToVector2(), cor.Color);
            }

            foreach (var cor in PontosRegressao)
            {
                var rect = new Rectangle(cor.Point.X - 2, cor.Point.Y - 2, 4, 4);
                spriteBatch.Draw(white, cor.Point.ToVector2(), cor.Color);
            }

            spriteBatch.End();

            base.Draw(gameTime);
        }
    }
}
