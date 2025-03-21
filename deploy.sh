#!/bin/bash

# ----------------------
# CONFIGURE THIS SECTION
# ----------------------

APP_NAME="myshinyapp"
ACR_NAME="resistentieprofielacr"
RESOURCE_GROUP="resistentieprofiel"
WEBAPP_NAME="resistentieprofiel"
IMAGE_NAME="$ACR_NAME.azurecr.io/$APP_NAME:latest"

# ----------------------
# 1. BUILD DOCKER IMAGE
# ----------------------

echo "🔧 Building Docker image..."
docker build --platform linux/amd64 -t $IMAGE_NAME .

if [ $? -ne 0 ]; then
  echo "❌ Docker build failed."
  exit 1
fi

# ----------------------
# 2. PUSH TO ACR
# ----------------------

echo "🔑 Logging into ACR..."
az acr login --name $ACR_NAME

echo "📤 Pushing Docker image to ACR..."
docker push $IMAGE_NAME

if [ $? -ne 0 ]; then
  echo "❌ Docker push failed."
  exit 1
fi

# ----------------------
# 3. DEPLOY TO AZURE WEB APP
# ----------------------

echo "🚀 Deploying to Azure Web App..."
az webapp config container set \
  --name resistentieprofiel \
  --resource-group resistentieprofiel \
  --container-image-name resistentieprofielacr.azurecr.io/myshinyapp:latest \
  --container-registry-url https://resistentieprofielacr.azurecr.io \
  --container-registry-user resistentieprofielacr \
  --container-registry-password $(az acr credential show --name $ACR_NAME --query "passwords[0].value" -o tsv)

if [ $? -ne 0 ]; then
  echo "❌ Azure deployment failed."
  exit 1
fi

# ----------------------
# 4. PUSH CHANGES TO GITHUB
# ----------------------

echo "📦 Adding and pushing changes to GitHub..."
git add .
git commit -m "Update Shiny app deployment"
git push origin main

if [ $? -ne 0 ]; then
  echo "❌ Git push failed (maybe no changes to commit?)."
else
  echo "✅ GitHub push complete."
fi

echo "🎉 Deployment complete!"
