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
        private Texture2D white;

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
                    Color = cores.TryGetValue(Algoritmo.resultado(modelo, MVector.Build.Dense(new[] { p.X / 100.0d, p.Y / 100.0d })), out var v) ? v : Color.White
                })
                .ToList();

            PontosXOR = dados
                .Select(par => new PointColor
                {
                    Point = new Point((int)(par.X[0] * 100), (int)(par.X[1] * 100)),
                    Color = cores[par.Y]
                });

            white = new Texture2D(GraphicsDevice, 1, 1);
            white.SetData(new [] { Color.White });
            // TODO: use this.Content to load your game content here
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
            GraphicsDevice.Clear(Color.CornflowerBlue);
            // TODO: Add your drawing code here
            spriteBatch.Begin();
            foreach (var cor in CoresFundo)
            {
                spriteBatch.Draw(white, cor.Point.ToVector2(), cor.Color);
            }

            foreach (var cor in PontosXOR)
            {
                spriteBatch.Draw(white, cor.Point.ToVector2(), cor.Color);
            }

            base.Draw(gameTime);
        }
    }
}
