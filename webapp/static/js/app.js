/**
 * Client-side JavaScript for managing the agents and chores lists,
 * and the multi-step dislike values flow.
 */

// ===== State Management =====
let currentStep = 1;
let totalSteps = 1;

// ===== Helper Functions =====

/**
 * Get all agent names from the DOM.
 * @returns {string[]} Array of agent names
 */
function getAgents() {
    return Array.from(document.querySelectorAll('#agents-list input[name="agents"]'))
        .map(input => input.value);
}

/**
 * Get all chore names from the DOM.
 * @returns {string[]} Array of chore names
 */
function getChores() {
    return Array.from(document.querySelectorAll('#chores-list input[name="chores"]'))
        .map(input => input.value);
}

/**
 * Escape HTML special characters to prevent XSS.
 * @param {string} str - The string to escape
 * @returns {string} The escaped string
 */
function escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
}

// ===== List Management =====

/**
 * Add a new agent to the agents list.
 */
function addAgent() {
    const input = document.getElementById('agent-input');
    const name = input.value.trim();

    if (name === '') {
        return;
    }

    addItemToList('agents-list', 'agents', name);
    input.value = '';
    input.focus();

    document.getElementById('agents-empty').style.display = 'none';
    updateNextButton();
}

/**
 * Add a new chore to the chores list.
 */
function addChore() {
    const input = document.getElementById('chore-input');
    const name = input.value.trim();

    if (name === '') {
        return;
    }

    addItemToList('chores-list', 'chores', name);
    input.value = '';
    input.focus();

    document.getElementById('chores-empty').style.display = 'none';
    updateNextButton();
}

/**
 * Add an item to a specified list.
 * @param {string} listId - The ID of the <ul> element
 * @param {string} inputName - The name attribute for the hidden input
 * @param {string} value - The value to add
 */
function addItemToList(listId, inputName, value) {
    const list = document.getElementById(listId);

    const li = document.createElement('li');

    const span = document.createElement('span');
    span.textContent = value;

    const hiddenInput = document.createElement('input');
    hiddenInput.type = 'hidden';
    hiddenInput.name = inputName;
    hiddenInput.value = value;

    const removeBtn = document.createElement('button');
    removeBtn.type = 'button';
    removeBtn.className = 'remove-btn';
    removeBtn.textContent = 'Remove';
    removeBtn.onclick = function() {
        removeItem(this);
    };

    li.appendChild(span);
    li.appendChild(hiddenInput);
    li.appendChild(removeBtn);

    list.appendChild(li);
}

/**
 * Remove an item from the list.
 * @param {HTMLElement} button - The remove button that was clicked
 */
function removeItem(button) {
    const li = button.parentElement;
    const list = li.parentElement;

    list.removeChild(li);

    if (list.children.length === 0) {
        const emptyId = list.id.replace('-list', '-empty');
        document.getElementById(emptyId).style.display = 'block';
    }

    updateNextButton();
}

// ===== Step Navigation =====

/**
 * Update the "Next" button state based on whether agents and chores exist.
 */
function updateNextButton() {
    const agents = getAgents();
    const chores = getChores();
    const nextBtn = document.getElementById('btn-next-to-values');

    if (nextBtn) {
        nextBtn.disabled = agents.length === 0 || chores.length === 0;
    }
}

/**
 * Update the progress indicator.
 */
function updateProgressIndicator() {
    const stepLabel = document.getElementById('step-label');
    const progressFill = document.getElementById('progress-fill');

    if (stepLabel) {
        stepLabel.textContent = `Step ${currentStep} of ${totalSteps}`;
    }

    if (progressFill) {
        const percentage = (currentStep / totalSteps) * 100;
        progressFill.style.width = `${percentage}%`;
    }
}

/**
 * Navigate to a specific step.
 * @param {number} stepNum - The step number to navigate to
 */
function goToStep(stepNum) {
    // Save current slider values before leaving
    saveDislikeValuesToHiddenInputs();

    // Hide all steps
    document.querySelectorAll('.step').forEach(el => el.classList.remove('active'));

    // Show target step
    const targetStep = document.getElementById(`step-${stepNum}`);
    if (targetStep) {
        targetStep.classList.add('active');
    }

    currentStep = stepNum;
    updateProgressIndicator();

    // Scroll to top of form
    window.scrollTo({ top: 0, behavior: 'smooth' });
}

/**
 * Transition from step 1 to the dislike value steps.
 */
function goToDislikeValues() {
    const agents = getAgents();
    const chores = getChores();

    if (agents.length === 0 || chores.length === 0) {
        alert('Please add at least one agent and one chore');
        return;
    }

    // Generate dislike value steps for each agent
    generateDislikeSteps(agents, chores);

    // Update total steps (step 1 + one step per agent)
    totalSteps = 1 + agents.length;

    // Navigate to step 2 (first agent)
    goToStep(2);
}

/**
 * Generate the HTML for each agent's dislike value form.
 * @param {string[]} agents - Array of agent names
 * @param {string[]} chores - Array of chore names
 */
function generateDislikeSteps(agents, chores) {
    const container = document.getElementById('dislike-steps-container');
    container.innerHTML = '';

    agents.forEach((agent, index) => {
        const stepNum = index + 2;
        const isLast = index === agents.length - 1;
        const nextAgent = isLast ? null : agents[index + 1];

        // Generate chore rating rows
        const choreRows = chores.map(chore => `
            <div class="chore-rating-row">
                <label>${escapeHtml(chore)}</label>
                <input
                    type="range"
                    min="1"
                    max="10"
                    value="5"
                    data-agent="${escapeHtml(agent)}"
                    data-chore="${escapeHtml(chore)}"
                    oninput="updateSliderValue(this)"
                >
                <span class="slider-value">5</span>
            </div>
        `).join('');

        // Generate navigation buttons
        let navButtons;
        if (isLast) {
            navButtons = `
                <button type="button" class="btn-back" onclick="goToStep(${stepNum - 1})">Back</button>
                <button type="submit" class="submit-btn">Calculate Allocation</button>
            `;
        } else {
            navButtons = `
                <button type="button" class="btn-back" onclick="goToStep(${stepNum - 1})">Back</button>
                <button type="button" class="btn-next" onclick="goToStep(${stepNum + 1})">
                    Next: ${escapeHtml(nextAgent)}'s Turn
                </button>
            `;
        }

        const stepHtml = `
            <div id="step-${stepNum}" class="step">
                <section class="dislike-section">
                    <h2>${escapeHtml(agent)}'s Dislike Values</h2>
                    <p class="section-description">
                        Rate how much ${escapeHtml(agent)} dislikes each chore
                    </p>
                    <div class="scale-hint">
                        <span>1 = Don't mind</span>
                        <span>10 = Hate it</span>
                    </div>
                    <div class="chore-ratings">
                        ${choreRows}
                    </div>
                </section>
                <div class="nav-buttons">
                    ${navButtons}
                </div>
            </div>
        `;

        container.insertAdjacentHTML('beforeend', stepHtml);
    });
}

/**
 * Update the displayed value next to a slider.
 * @param {HTMLInputElement} slider - The range input element
 */
function updateSliderValue(slider) {
    const valueDisplay = slider.nextElementSibling;
    if (valueDisplay) {
        valueDisplay.textContent = slider.value;
    }
}

/**
 * Save all slider values to hidden inputs for form submission.
 */
function saveDislikeValuesToHiddenInputs() {
    const container = document.getElementById('dislike-hidden-inputs');
    const sliders = document.querySelectorAll('.chore-rating-row input[type="range"]');

    sliders.forEach(slider => {
        const agent = slider.dataset.agent;
        const chore = slider.dataset.chore;
        const value = slider.value;
        const inputName = `dislike_values[${agent}][${chore}]`;

        // Check if hidden input already exists
        let existing = container.querySelector(`input[name="${CSS.escape(inputName)}"]`);

        if (existing) {
            existing.value = value;
        } else {
            const hidden = document.createElement('input');
            hidden.type = 'hidden';
            hidden.name = inputName;
            hidden.value = value;
            container.appendChild(hidden);
        }
    });
}

// ===== Form Validation =====

/**
 * Validate the form before submission.
 * @param {Event} event - The form submit event
 * @returns {boolean} - Whether the form should submit
 */
function validateForm(event) {
    // Save dislike values before submission
    saveDislikeValuesToHiddenInputs();

    const agents = getAgents();
    const chores = getChores();

    const errors = [];

    if (agents.length === 0) {
        errors.push('At least one agent is required');
    }

    if (chores.length === 0) {
        errors.push('At least one chore is required');
    }

    // Check if we have dislike values (should have been through the flow)
    const hiddenInputs = document.querySelectorAll('#dislike-hidden-inputs input');
    if (hiddenInputs.length === 0) {
        errors.push('Please enter dislike values for all agents');
    }

    if (errors.length > 0) {
        event.preventDefault();
        alert(errors.join('\n'));
        return false;
    }

    return true;
}

// ===== Initialization =====

document.addEventListener('DOMContentLoaded', function() {
    // Agent input - add on Enter
    const agentInput = document.getElementById('agent-input');
    if (agentInput) {
        agentInput.addEventListener('keypress', function(event) {
            if (event.key === 'Enter') {
                event.preventDefault();
                addAgent();
            }
        });
    }

    // Chore input - add on Enter
    const choreInput = document.getElementById('chore-input');
    if (choreInput) {
        choreInput.addEventListener('keypress', function(event) {
            if (event.key === 'Enter') {
                event.preventDefault();
                addChore();
            }
        });
    }

    // Form validation on submit
    const form = document.getElementById('allocation-form');
    if (form) {
        form.addEventListener('submit', validateForm);
    }

    // Initialize button state and progress indicator
    updateNextButton();
    updateProgressIndicator();
});
