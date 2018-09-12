using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using System;
using System.Collections.Generic;

//Aliases
using Realizacao = RedeAmarildo.Algoritmo.Realizacao;
using FMatrix = MathNet.Numerics.LinearAlgebra.Matrix<double>;

namespace RedeMyLittlePoney.Plot
{
    /// <summary>
    /// This is the main type for your game.
    /// </summary>
    public class Game1 : Game
    {
        int tamanho = 10;

        float margem = 0.05f;

        Texture2D tex;

        Realizacao realizacao = Algoritmo.algoritmoCustom.Melhor;

        Dictionary<FMatrix, Color> cores = new Dictionary<FMatrix, Color>
        {
            { Algoritmo.matrizLinha(new[]{ 1.0, 0, 0 }), Color.Red },
            { Algoritmo.matrizLinha(new[]{ 0, 1.0, 0 }), Color.Green },
            { Algoritmo.matrizLinha(new[]{ 0, 0, 1.0 }), Color.Blue },
        };

        #region MonoGame Specific
        GraphicsDeviceManager graphics;
        SpriteBatch spriteBatch;

        public Game1()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";

            Window.AllowUserResizing = true;
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

            tex = new Texture2D(GraphicsDevice, 1, 1);
            tex.SetData(new[] { Color.White });



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
        #endregion

        /// <summary>
        /// This is called when the game should draw itself.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.CornflowerBlue);

            spriteBatch.Begin();

            //spriteBatch.Draw()
            // TODO: Add your drawing code here
            int w = GraphicsDevice.Viewport.Width, h = GraphicsDevice.Viewport.Height;
            int y0 = (int)(h * margem), yn = h - y0;

            int tamanhoEfetivo = Math.Abs(y0 - yn);

            float escala = tamanhoEfetivo / tamanho;

            int x0 = w / 2 - tamanhoEfetivo / 2, xn = x0 + tamanhoEfetivo;

            int espacamento = tamanhoEfetivo / tamanho;

            spriteBatch.Draw(tex, new Rectangle(x0, y0, tamanhoEfetivo, tamanhoEfetivo), Color.White);

            for (int x = x0; x < xn; x += espacamento)
            {
                spriteBatch.Draw(tex, new Rectangle(x, y0, tamanhoEfetivo, 1), null, Color.LightGray, MathHelper.Pi / 2, Vector2.Zero, SpriteEffects.None, 1.0f);
            }

            for (int y = y0; y < yn; y += espacamento)
            {
                spriteBatch.Draw(tex, new Rectangle(x0, y, tamanhoEfetivo, 1), null, Color.Gray, 0f, Vector2.Zero, SpriteEffects.None, 1.0f);
            }

            for (int x = 0; x < tamanhoEfetivo; x++)
            {
                for (int y = 0; y < tamanhoEfetivo; y++)
                {
                    var vx = Algoritmo.matrizLinha(new[] { 1.0, x / escala, y / escala });
                    var vy = Algoritmo.saida(realizacao.W, vx);

                    if (cores.ContainsKey(vy))
                    {
                        var cor = new Color(cores[vy], 0.2f);

                        spriteBatch.Draw(tex, new Rectangle(x0 + x, y0 + y, 1, 1), cor);
                    }

                }
            }

            foreach (var dado in Algoritmo.algoritmoCustom.Dados)
            {
                var row = dado.X.Row(0);
                int x = (int)(row[1] * escala), y = (int)(row[2] * escala);
                var rect = new Rectangle(x + x0 - 3, y + y0 - 3, 7, 7);
                var border = new Rectangle(rect.X - 1, rect.Y - 1, rect.Width + 2, rect.Height + 2);

                var cor = new Color(cores[dado.Y], 0.8f);

                spriteBatch.Draw(tex, border, Color.Black);
                spriteBatch.Draw(tex, rect, cor);
            }

            spriteBatch.End();

            base.Draw(gameTime);
        }
    }
}
