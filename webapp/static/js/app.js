/**
 * Client-side JavaScript for managing the agents and chores lists.
 * Handles adding and removing items without server requests.
 */

/**
 * Add a new agent to the agents list.
 * Reads the value from the input field and creates a new list item.
 */
function addAgent() {
    const input = document.getElementById('agent-input');
    const name = input.value.trim();

    if (name === '') {
        return; // Don't add empty names
    }

    addItemToList('agents-list', 'agents', name);
    input.value = ''; // Clear the input
    input.focus(); // Keep focus for quick entry

    // Hide the empty message
    document.getElementById('agents-empty').style.display = 'none';
}

/**
 * Add a new chore to the chores list.
 * Reads the value from the input field and creates a new list item.
 */
function addChore() {
    const input = document.getElementById('chore-input');
    const name = input.value.trim();

    if (name === '') {
        return; // Don't add empty names
    }

    addItemToList('chores-list', 'chores', name);
    input.value = ''; // Clear the input
    input.focus(); // Keep focus for quick entry

    // Hide the empty message
    document.getElementById('chores-empty').style.display = 'none';
}

/**
 * Add an item to a specified list.
 * Creates the list item HTML with a hidden input for form submission.
 *
 * @param {string} listId - The ID of the <ul> element
 * @param {string} inputName - The name attribute for the hidden input
 * @param {string} value - The value to add
 */
function addItemToList(listId, inputName, value) {
    const list = document.getElementById(listId);

    // Create the list item
    const li = document.createElement('li');

    // Create the display span
    const span = document.createElement('span');
    span.textContent = value;

    // Create the hidden input (for form submission)
    const hiddenInput = document.createElement('input');
    hiddenInput.type = 'hidden';
    hiddenInput.name = inputName;
    hiddenInput.value = value;

    // Create the remove button
    const removeBtn = document.createElement('button');
    removeBtn.type = 'button';
    removeBtn.className = 'remove-btn';
    removeBtn.textContent = 'Remove';
    removeBtn.onclick = function() {
        removeItem(this);
    };

    // Assemble the list item
    li.appendChild(span);
    li.appendChild(hiddenInput);
    li.appendChild(removeBtn);

    // Add to the list
    list.appendChild(li);
}

/**
 * Remove an item from the list.
 * Called when the "Remove" button is clicked.
 *
 * @param {HTMLElement} button - The remove button that was clicked
 */
function removeItem(button) {
    const li = button.parentElement;
    const list = li.parentElement;

    // Remove the list item
    list.removeChild(li);

    // Show empty message if list is now empty
    if (list.children.length === 0) {
        const emptyId = list.id.replace('-list', '-empty');
        document.getElementById(emptyId).style.display = 'block';
    }
}

/**
 * Validate the form before submission.
 * Ensures at least one agent and one chore are present.
 *
 * @param {Event} event - The form submit event
 * @returns {boolean} - Whether the form should submit
 */
function validateForm(event) {
    const agentsList = document.getElementById('agents-list');
    const choresList = document.getElementById('chores-list');

    const agentsCount = agentsList.children.length;
    const choresCount = choresList.children.length;

    const errors = [];

    if (agentsCount === 0) {
        errors.push('At least one agent is required');
    }

    if (choresCount === 0) {
        errors.push('At least one chore is required');
    }

    if (errors.length > 0) {
        event.preventDefault();
        alert(errors.join('\n'));
        return false;
    }

    return true;
}

/**
 * Allow adding items by pressing Enter in the input fields.
 */
document.addEventListener('DOMContentLoaded', function() {
    // Agent input - add on Enter
    const agentInput = document.getElementById('agent-input');
    if (agentInput) {
        agentInput.addEventListener('keypress', function(event) {
            if (event.key === 'Enter') {
                event.preventDefault(); // Prevent form submission
                addAgent();
            }
        });
    }

    // Chore input - add on Enter
    const choreInput = document.getElementById('chore-input');
    if (choreInput) {
        choreInput.addEventListener('keypress', function(event) {
            if (event.key === 'Enter') {
                event.preventDefault(); // Prevent form submission
                addChore();
            }
        });
    }

    // Form validation on submit
    const form = document.getElementById('allocation-form');
    if (form) {
        form.addEventListener('submit', validateForm);
    }
});
