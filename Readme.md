# 📝 Haskell Todo List App

A minimal full-stack **Todo List** web app built with:

- **Haskell** (GHC 9.6) & **Servant** – type-safe REST API  
- **Lucid** – DSL for HTML rendering  
- **HTMX** – AJAX interactivity without JavaScript  
- **Tailwind CSS** – utility-first responsive design  
- **PostgreSQL** – durable storage  
- **Persistent** – typed Haskell ORM  

---

<!-- Live link https://todo-app-haskell.onrender.com-->
### 🌐 Live Demo

Check out the live version of the app here: [Todo App Live Demo](https://todo-app-haskell.onrender.com)

[https://todo-app-haskell.onrender.com](https://todo-app-haskell.onrender.com)

## 🚀 Features

- create, toggle, and delete todos  
- server-rendered interactivity powered by HTMX  
- responsive mobile-first layout with Tailwind  
- clean, type-safe codebase with Haskell  

---

## 🏁 Getting Started (Local Development)

### Prerequisites

- Docker  
- PostgreSQL (for local PostgreSQL setup)  


### ✅ Run Locally
 - Export `DATABASE_URL` and `PORT` environment variables.
```bash
export DATABASE_URL="postgresql://postgres:postgres@localhost:5432/postgres"
export PORT=8000
```
 - Install dependencies using Cabal.

```bash
cabal update
cabal install --only-dependencies
```
- Build the app using Cabal.

```bash
cabal build
```
 - Run the app using Cabal.
    
```bash
cabal run
```

### ✅ Run with Docker

```bash
# Build Docker image
docker build -t todo-app .
```

# Run PostgreSQL and the app
```bash
docker run --name todo-postgres -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres:15
docker run \
  --name todo-app --rm \
  -e DATABASE_URL="postgresql://postgres:postgres@host.docker.internal:5432/postgres" \
  -e PORT=8000 \
  -p 8000:8000 \
  todo-app


Open http://localhost:8000 in your browser.