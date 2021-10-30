from django.shortcuts import render

# Create your views here.
def index(request):
    return render(request, 'index.html')

def profile(request):
    return render(request, 'deport.html')

def visuals(request):
    return render(request, 'visuals.html')